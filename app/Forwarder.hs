{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Lib

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.Maybe
import           Protolude
import           System.Posix.Unistd            ( usleep )

newtype App a = App { runApp :: LoggingT IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadLogger)

main :: IO ()
main = runStdoutLoggingT (runApp run)

run :: App ()
run = do
  dev <- fromJust <$> newDriver "0000:02:00.0" 1 1
  readPackets dev
 where
  readPackets = evalStateT
    (do
      packets <- receive 0 64
      liftIO $ do
        putStrLn (show packets :: Text)
        usleep 1000000
    )
