{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( Bot (..),
      HirculesConfig (..),
      Net,
      commands,
      hasURLs,
      lookupURLTitles,
      privmsg,
      isSearchReplace,
      handleSearchReplace,
      uptime,
      write
    ) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Yaml
import           Network
import           System.Exit
import           System.IO
import           System.Time
import           Text.Printf
import           Text.Regex.PCRE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.HTML.Scalpel as Sc
import qualified Text.Regex as RC

data Bot = Bot 
  { socket :: Handle
  , conf :: HirculesConfig
  , starttime :: ClockTime }

type Net = ReaderT Bot IO

data HirculesConfig = HirculesConfig 
  { server :: T.Text
  , port :: Integer
  , nick :: T.Text
  , password :: T.Text -- Password for the nickname on the irc network
  , chans :: T.Text
  , commandChar :: T.Text
  } deriving Show

instance FromJSON HirculesConfig where
  parseJSON (Object v) = HirculesConfig <$>
                         v .: "server" <*>
                         v .: "port" <*>
                         v .: "nick" <*>
                         v .: "password" <*>
                         v .: "chans" <*>
                         v .: "commandChar"
  parseJSON _ = mzero

commands =
  [  -- ("quit"   , ("Quits the server"                 , handleQuit)), 
     ("help"    , ("Print out the help message"                    , handleHelp))
   , ("echo"    , ("Echo back the same string"                     , handleEcho))
   , ("join"    , ("Join a channel/channels. Eg. !join #foo,#bar"  , handleJoin))
   , ("uptime"  , ("Show the running time of the bot"              , handleUptime))
  ]

handleQuit n c l    =  write "QUIT" ":Exiting" >> liftIO exitSuccess

handleUptime n c l  =  uptime >>= privmsg n c

handleEcho          =  privmsg

handleHelp n c l
  | not (null . drop 1 $ T.splitOn " " l)
  = case _c of
    Just (message, _) -> privmsg n c $ command <> " - " <> message
    Nothing           -> privmsg n c "Command not found"
  where
    _c = lookup command commands
    command = head (T.splitOn " " l)

handleHelp n c l
  | null . drop 1 $ T.splitOn " " l
  = mapM_ printhelp commands
  where
    printhelp (command, (help, _)) = privmsg n c $ command <> " - " <> help

handleJoin n c l
  = case T.splitOn " " l of
    [chans] -> write "JOIN" chans
    _ -> privmsg n c l
  where
    printhelp c = case lookup c commands of
                  Just (help, _) -> privmsg n c $ c <> " - " <> help
                  Nothing -> return ()

privmsg :: T.Text -> T.Text -> T.Text -> Net ()
privmsg _ chan s
  | "#" `T.isPrefixOf` chan
  = write "PRIVMSG" (chan <> " :" <> s)
privmsg nick _ s
  = write "PRIVMSG" (T.takeWhile (/= '!') nick <> " :" <> s)

write :: T.Text -> T.Text -> Net ()
write _s _t = 
  let s = T.unpack _s
      t = T.unpack _t
    in do
      h <- asks socket
      liftIO $ hPrintf h "%s %s\r\n" s t
      liftIO $ printf "%s %s\r\n" s t

uptime :: Net T.Text
uptime = do
  now <- liftIO getClockTime
  zero <- asks starttime
  return . prettyTime $ diffClockTimes now zero

prettyTime :: TimeDiff -> T.Text
prettyTime td = T.pack $
  unwords $ map (uncurry (++) . first show) $ 
  if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter ((/= 0) . fst) $ reverse $ snd $
                  foldl merge (tdSec td,[]) metrics

hasURLs :: T.Text -> Bool
hasURLs s = "http://" `T.isInfixOf` s || "https://" `T.isInfixOf` s 

lookupURLTitles :: T.Text -> T.Text -> T.Text -> Net ()
lookupURLTitles nick chan s = do
  titles <- liftIO $ mapM scrapeTitle urls
  mapM_ (privmsg nick chan . T.filter (/= '\r') . T.unwords . T.lines) $ catMaybes titles
 where
    urls = filter isURL words
    words = T.splitOn " " s
    isURL s = "http://" `T.isPrefixOf` s ||
              "https://" `T.isPrefixOf` s

scrapeTitle :: T.Text -> IO (Maybe T.Text)
scrapeTitle u = Sc.scrapeURL (T.unpack u) (Sc.text ("title" :: String))

{-TODO: No doubt these regexes are broken in some (many) ways...-}
{-TODO: Use lookbehinds for the s/// slashes: http://www.rexegg.com/regex-lookarounds.html-}
searchReplaceRegexSelf :: String
searchReplaceRegexSelf = "^\\s*s/[^/]+?/[^/]+?/"  -- "s/foo/bar/"

searchReplaceRegex_SearchTerm :: String
searchReplaceRegex_SearchTerm = "^\\s*s/([^/]+?)/[^/]+?/"  -- "s/(foo)/bar/" Group on the search term
searchReplaceRegex_ReplaceTerm :: String
searchReplaceRegex_ReplaceTerm = "^\\s*s/[^/]+?/([^/]+?)/"  -- "s/foo/(bar)/" Group on the replace term

searchReplaceRegexNick :: String
-- Allowed nick chars:  a-z A-Z 0-9 _ - \ [ ] { } ^ ` |
searchReplaceRegexNick = "^\\s*[a-zA-Z0-9_\\-\\\\\\[\\]\\{\\}^`|]+?:\\s*s/[^/]+?/[^/]+?/" -- "somenick: s/foo/bar/"
nickRegex :: String
nickRegex = "^\\s*[a-zA-Z0-9_\\-\\\\\\[\\]\\{\\}^`|]+?:" -- "somenick:"

-- Check if this is a search replace line eg:
isSearchReplace :: T.Text -> Bool
isSearchReplace s = 
  ((T.unpack s =~ searchReplaceRegexSelf) :: Bool) ||
  ((T.unpack s =~ searchReplaceRegexNick) :: Bool)

handleSearchReplace :: FilePath -> T.Text -> T.Text -> T.Text -> Net ()
handleSearchReplace log _nick chan s
  | (T.unpack s =~ searchReplaceRegexNick) :: Bool,
    (stripTargetNick s =~ searchReplaceRegex_SearchTerm) :: Bool,
    (stripTargetNick s =~ searchReplaceRegex_ReplaceTerm) :: Bool
  = let targetnick = T.takeWhile (/= ':') $ T.pack ((T.unpack s =~ nickRegex) :: String)
        rest = stripTargetNick s
        searchTerm  = T.pack $ (head $ (rest =~ searchReplaceRegex_SearchTerm :: [[String]])) !! 1
        replaceTerm = T.pack $ (head $ (rest =~ searchReplaceRegex_ReplaceTerm :: [[String]])) !! 1
        regexp = targetnick <> ".*? PRIVMSG .*?:"
      in do
        _matches <- liftIO $ grep log regexp
        let notSearchReplaceLine = not . isSearchReplace . stripPrelude
            matches = filter notSearchReplaceLine _matches
          in
            unless (null matches) $
              let line = stripPrelude (last matches)
              in do
                result <- liftIO $ regexReplace searchTerm line replaceTerm
                case result of
                  Left e -> privmsg _nick "" e
                  Right t -> privmsg "" chan $ nickOnly targetnick <> " meant to say: " <> t

-- AA TODO: Refactor this out to remove duplication
handleSearchReplace log _nick chan s
  | (T.unpack s =~ searchReplaceRegexSelf) :: Bool,
    (T.unpack s =~ searchReplaceRegex_SearchTerm) :: Bool,
    (T.unpack s =~ searchReplaceRegex_ReplaceTerm) :: Bool
  = let searchTerm  = T.pack $ (head (T.unpack s =~ searchReplaceRegex_SearchTerm :: [[String]])) !! 1
        replaceTerm = T.pack $ (head (T.unpack s =~ searchReplaceRegex_ReplaceTerm :: [[String]])) !! 1
        regexp = _nick <> ".*? PRIVMSG .*?:.*?" <> searchTerm
      in do
        _matches <- liftIO $ grep log regexp
        let notSearchReplaceLine = not . isSearchReplace . stripPrelude
            matches = filter notSearchReplaceLine _matches
          in
            unless (null matches) $
              let line = stripPrelude (last matches)
              in do
                result <- liftIO $ regexReplace searchTerm line replaceTerm
                case result of
                  Left e -> privmsg _nick "" e
                  Right t -> do
                    liftIO $ T.putStrLn $ "line: " <> line
                    liftIO $ T.putStrLn $ "searchTerm: " <> searchTerm
                    liftIO $ T.putStrLn $ "replaceTerm: " <> replaceTerm
                    liftIO $ T.putStrLn $ "newtext: " <> t
                    privmsg "" chan $ nickOnly _nick <> " meant to say: " <> t

nickOnly :: T.Text -> T.Text
nickOnly = T.takeWhile (/= '!') . T.dropWhile (== ':')

stripPrelude = T.drop 1 . T.dropWhile (/= ':') . T.dropWhile (/= ' ')

-- "somnick: s/foo/bar/" -> " s/foo/bar/"
stripTargetNick t = T.unpack . T.strip . T.drop 1 . T.dropWhile (/= ':') $ t

isValidRegex :: T.Text -> Bool
isValidRegex r = undefined

regexReplace :: T.Text -> T.Text -> T.Text -> IO (Either T.Text T.Text)
regexReplace _regex _str _replace = 
  let regex = T.unpack _regex
      str = T.unpack _str
      replace = T.unpack _replace
    in catch (return $ Right $! T.pack $! RC.subRegex (RC.mkRegex regex) str replace)
             (\e -> return $ Left $ T.pack $ show (e :: SomeException))

{-TODO: Incredibly naive and probably slow as hell-}
grep :: FilePath -> T.Text -> IO [T.Text]
grep file regexp
  | regexp /= ""
  = case makeRegexM (T.unpack regexp) :: Maybe Regex of
      Nothing -> return []
      _ -> do filecontents <- readFile file
              let _lines = lines filecontents
                in return $ map T.pack $ filter (\s -> s =~ T.unpack regexp :: Bool) _lines
