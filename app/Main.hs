{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib

import           Control.Arrow
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Data.List
import           Data.List.Split
import           Data.Monoid
import           Data.Yaml
import           Network
import           System.Directory
import           System.IO
import           System.Posix.Files
import           System.Time
import           Text.Printf
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T


fifoname = ".hircules-fifo"
logfile = ".hircules-log"

main :: IO ()
main = do
  yamldata <- BS.readFile "config.yaml"
  case decode yamldata :: Maybe [HirculesConfig] of
    Just [conf] -> bracket (connect conf) disconnect loop
        where
          disconnect = hClose . socket
          loop = runReaderT (run conf)
    Just cs -> putStrLn $ "Error in config.yaml: " ++ show cs
    Nothing -> putStrLn "Error in config.yaml"

connect :: HirculesConfig -> IO Bot
connect conf = notify $ do
  t <- getClockTime
  h <- connectTo (T.unpack $ server conf) (PortNumber (fromIntegral (port conf)))
  hSetBuffering h NoBuffering
  return (Bot h conf t)
 where
  notify = bracket_
      (printf "Connecting to %s ... " (T.unpack $ server conf) >> hFlush stdout)
      (putStrLn "done.")

run :: HirculesConfig -> Net ()
run conf = do
  privmsg "NickServ" "" ("GHOST " <> 
                         nick conf <> " " <> 
                         password conf)
  write "NICK" $ nick conf
  write "USER" $ nick conf <> " 0 * :hircules bot"
  privmsg "NickServ" "" ("IDENTIFY " <> 
                         password conf <> " ")
  write "JOIN" $ chans conf
  asks socket >>= listen

listen :: Handle -> Net ()
listen h = do
  fileexists <- liftIO $ doesFileExist fifoname
  when fileexists (liftIO $ removeFile fifoname)
  _ <- liftIO $ createNamedPipe fifoname accessModes
  let processIRC = forever $ do
        s <- T.init `fmap` liftIO (T.hGetLine h)
        liftIO (T.putStrLn s)
        liftIO (T.appendFile logfile $ s <> "\n")
        if ping s 
        then pong s 
        else when (isprivmsg s) $
                  let (n, c, l) = splitprivmsg s
                  in eval n c l
       where
          clean = T.drop 2 . T.dropWhile (/= ' ') . T.drop 1 . T.dropWhile (/= ' ') . T.drop 1 . T.dropWhile (/= ' ')
          isprivmsg = T.isPrefixOf "PRIVMSG" . T.drop 1 . T.dropWhile (/= ' ') . T.drop 1 
          splitprivmsg s =
            (n, c, line)
            where
              [n, _, c] = take 3 $ T.splitOn " " s
              line = clean s
          ping x = "PING :" `T.isPrefixOf` x
          pong x = write "PONG" (":" <> T.drop 6 x)

      processFIFO = do
        _conf <- asks conf
        s <- liftIO $ openFile fifoname ReadWriteMode >>= T.hGetContents
        mapM_ (privmsg "" $ chans _conf) $ T.lines s

    in do 
      bot <- ask
      liftIO $ concurrently (runReaderT processIRC bot)
                            (runReaderT processFIFO bot)
      return ()

eval :: T.Text -> T.Text -> T.Text -> Net ()
eval nickname chan line = do
  _conf <- asks conf
  unless (nickname == nick _conf) $
    case commandChar _conf `T.isPrefixOf` line of
      True -> case lookup command commands of
                Just (docs, f) -> f nickname chan args
                Nothing -> privmsg nickname chan "Command not found."
            where
              command = T.takeWhile (/= ' ') $ T.drop 1 line
              args = T.drop 1 $ T.dropWhile (/= ' ') line
      False -> do
        when (hasURLs line) $ lookupURLTitles nickname chan line
        when (isSearchReplace line) $ handleSearchReplace logfile nickname chan line
