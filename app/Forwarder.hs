{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Lib

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Logger
import qualified Data.ByteString               as B
import           Data.IORef
import           Data.Maybe
import qualified Data.Vector                   as V
import           Protolude
import           System.Clock

newtype App a = App { runApp :: LoggingT IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadLogger)

main :: IO ()
main = runStdoutLoggingT (runApp run)

run :: App ()
run = do
  dev1    <- fromJust <$> newDriver "0000:02:00.0" 1 1
  dev2    <- fromJust <$> newDriver "0000:02:00.1" 1 1
  counter <- liftIO $ newIORef (0 :: Int)
  liftIO $ loop counter dev1 dev2

loop :: IORef Int -> Device -> Device -> IO ()
loop counter dev1 dev2 = do
  let clock = Monotonic
  timeRef <- newIORef (TimeSpec {sec = 0, nsec = 0})
  forever $ do
    forward dev1 dev2
    forward dev2 dev1
    c <- readIORef counter
    when
      (c .&. 0xFF == 0)
      (do
        t          <- getTime clock
        beforeTime <- readIORef timeRef
        when
          (sec (diffTimeSpec t beforeTime) >= 1)
          (do
            st1 <- stats dev1
            st2 <- stats dev2
            putStrLn (show st1 :: Text)
            putStrLn (show st2 :: Text)
          )
        writeIORef timeRef t
      )
    modifyIORef' counter (+ 1)

forward :: Device -> Device -> IO ()
forward rxDev txDev = do
  pkts <- receive rxDev (QueueId 0) 32
  unless
    (V.null pkts)
    (do
      let touchedPkts = map touchPacket pkts
      _ <- send txDev (QueueId 0) touchedPkts
      return ()
    )
  return ()
  where touchPacket b = (B.head b + 1) `B.cons` B.tail b
