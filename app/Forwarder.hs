{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Lib

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.Maybe
import qualified Data.Vector                   as V
import           Protolude

newtype App a = App { runApp :: LoggingT IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadLogger)

main :: IO ()
main = runStdoutLoggingT (runApp run)

run :: App ()
run = do
  dev1 <- fromJust <$> newDriver "0000:02:00.0" 1 1
  dev2 <- fromJust <$> newDriver "0000:02:00.1" 1 1
  loop dev1 dev2

loop :: Device -> Device -> App ()
loop dev1 dev2 = forever $ do
  forward dev1 dev2
  forward dev2 dev1

forward :: Device -> Device -> App ()
forward rxDev txDev = do
  pkts   <- liftIO $ receive rxDev (QueueId 0) 64
  result <- liftIO $ send txDev (QueueId 0) pkts
  unless
    (V.null pkts)
    (do
      liftIO $ putStrLn =<< dump txDev
      case result of
        Left  _ -> liftIO $ putStrLn ("Some packets were ignored." :: Text)
        Right _ -> liftIO $ putStrLn ("All packets were sent." :: Text)
    )
