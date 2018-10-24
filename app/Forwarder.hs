{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Lib

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.IORef
import           Data.Maybe
import qualified Data.Vector                   as V
import           Protolude

newtype App a = App { runApp :: LoggingT IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadLogger)

main :: IO ()
main = runStdoutLoggingT (runApp run)

run :: App ()
run = do
  dev1    <- fromJust <$> newDriver "0000:02:00.0" 1 1
  dev2    <- fromJust <$> newDriver "0000:02:00.1" 1 1
  counter <- liftIO $ newIORef (0 :: Int)
  loop counter dev1 dev2

loop :: IORef Int -> Device -> Device -> App ()
loop counter dev1 dev2 = forever $ do
  liftIO $ forward dev1 dev2
  liftIO $ forward dev2 dev1
  liftIO $ modifyIORef' counter (+ 1)
  c <- liftIO $ readIORef counter
  when
    (c .&. 0xFFF == 0)
    (liftIO $ do
      st1 <- stats dev1
      st2 <- stats dev2
      putStrLn (show st1 :: Text)
      putStrLn (show st2 :: Text)
    )

forward :: Device -> Device -> IO ()
forward rxDev txDev = do
  pkts <- receive rxDev (QueueId 0) 64
  _    <- send txDev (QueueId 0) pkts
  return ()
