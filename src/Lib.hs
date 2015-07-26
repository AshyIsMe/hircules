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
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Yaml
import           Network
import           System.Exit
import           System.IO
import           System.Time
import           Text.Printf
import           Text.Regex.PCRE
import qualified Text.Regex as RC
import qualified Data.Text as T
import qualified Text.HTML.Scalpel as Sc

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
  , commandChar :: Char
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
  | not (null . drop 1 $ splitOn " " l)
  = case _c of
    Just (message, _) -> privmsg n c $ command ++ " - " ++ message
    Nothing           -> privmsg n c "Command not found"
  where
    _c = lookup command commands
    command = head (splitOn " " l)

handleHelp n c l
  | null . drop 1 $ splitOn " " l
  = mapM_ printhelp commands
  where
    printhelp (command, (help, _)) = privmsg n c $ command ++ " - " ++ help

handleJoin n c l
  = case splitOn " " l of
    [chans] -> write "JOIN" chans
    _ -> privmsg n c l
  where
    printhelp c = case lookup c commands of
                  Just (help, _) -> privmsg n c $ c ++ " - " ++ help
                  Nothing -> return ()

privmsg :: String -> String -> String -> Net ()
privmsg _ chan s
  | "#" `isPrefixOf` chan
  = write "PRIVMSG" (chan ++ " :" ++ s)
privmsg nick _ s
  = write "PRIVMSG" (takeWhile (/= '!') nick ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  liftIO $ hPrintf h "%s %s\r\n" s t
  liftIO $ printf "%s %s\r\n" s t

uptime :: Net String
uptime = do
  now <- liftIO getClockTime
  zero <- asks starttime
  return . prettyTime $ diffClockTimes now zero

prettyTime :: TimeDiff -> String
prettyTime td =
  unwords $ map (uncurry (++) . first show) $ 
  if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter ((/= 0) . fst) $ reverse $ snd $
                  foldl' merge (tdSec td,[]) metrics

hasURLs :: String -> Bool
hasURLs s = "http://" `isInfixOf` s || "https://" `isInfixOf` s 

lookupURLTitles :: String -> String -> String -> Net ()
lookupURLTitles nick chan s = do
  titles <- liftIO $ mapM scrapeTitle urls
  mapM_ (privmsg nick chan . filter (/= '\r') . unwords . lines) $ catMaybes titles
 where
    urls = filter isURL words
    words = splitOn " " s
    isURL s = "http://" `isPrefixOf` s ||
              "https://" `isPrefixOf` s

scrapeTitle :: String -> IO (Maybe String)
scrapeTitle u = Sc.scrapeURL u (Sc.text ("title" :: String))

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
isSearchReplace :: String -> Bool
isSearchReplace s = 
  ((s =~ searchReplaceRegexSelf) :: Bool) ||
  ((s =~ searchReplaceRegexNick) :: Bool)

handleSearchReplace :: FilePath -> String -> String -> String -> Net ()
handleSearchReplace log _nick chan s
  | (s =~ searchReplaceRegexNick) :: Bool,
    (stripTargetNick s =~ searchReplaceRegex_SearchTerm) :: Bool,
    (stripTargetNick s =~ searchReplaceRegex_ReplaceTerm) :: Bool
  = let targetnick = takeWhile (/= ':') $ s =~ nickRegex :: String
        searchTerm  = (head $ ((stripTargetNick s) =~ searchReplaceRegex_SearchTerm :: [[String]])) !! 1
        replaceTerm = (head $ ((stripTargetNick s) =~ searchReplaceRegex_ReplaceTerm :: [[String]])) !! 1
        regexp = ":" ++ targetnick ++ ".*? PRIVMSG .*?:"
      in do
        _matches <- liftIO $ grep log regexp
        let notSearchReplaceLine = not . isSearchReplace . stripPrelude
            matches = filter notSearchReplaceLine _matches
          in
            unless (null matches) $
              let line = stripPrelude (last matches)
              in do
                newtext <- liftIO $ regexReplace searchTerm line replaceTerm
                privmsg "" chan $ nickOnly targetnick ++ " meant to say: " ++ newtext


handleSearchReplace log _nick chan s
  | (s =~ searchReplaceRegexSelf) :: Bool,
    (s =~ searchReplaceRegex_SearchTerm) :: Bool,
    (s =~ searchReplaceRegex_ReplaceTerm) :: Bool
  = let searchTerm  = (head $ (s =~ searchReplaceRegex_SearchTerm :: [[String]])) !! 1
        replaceTerm = (head $ (s =~ searchReplaceRegex_ReplaceTerm :: [[String]])) !! 1
        regexp = ":" ++ _nick ++ ".*? PRIVMSG .*?:"
      in do
        _matches <- liftIO $ grep log regexp
        let notSearchReplaceLine = not . isSearchReplace . stripPrelude
            matches = filter notSearchReplaceLine _matches
          in
            unless (null matches) $
              let line = stripPrelude (last matches)
              in do
                newtext <- liftIO $ regexReplace searchTerm line replaceTerm
                liftIO $ putStrLn $ "line: " ++ line
                liftIO $ putStrLn $ "searchTerm: " ++ searchTerm
                liftIO $ putStrLn $ "replaceTerm: " ++ replaceTerm
                liftIO $ putStrLn $ "newtext: " ++ newtext
                privmsg "" chan $ nickOnly _nick ++ " meant to say: " ++ newtext

nickOnly :: String -> String
nickOnly = takeWhile (/= '!') . dropWhile (== ':')

stripPrelude = drop 1 . dropWhile (/= ':') . drop 1

-- "somnick: s/foo/bar/" -> " s/foo/bar/"
stripTargetNick = drop 1 . dropWhile (/= ':')

{-TODO: This should return an Either SomeRegexError String-}
regexReplace regex str replace = 
  catch (return $! RC.subRegex (RC.mkRegex regex) str replace)
        (\e -> return $ show (e :: SomeException))

{-TODO: Incredibly naive and probably slow as hell-}
grep :: FilePath -> String -> IO [String]
grep file regexp
  | regexp /= ""
  = do 
    filecontents <- readFile file
    let _lines = lines filecontents
      in return $ filter (\s -> s =~ regexp :: Bool) _lines
