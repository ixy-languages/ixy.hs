{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Lib

import           Control.Monad.Catch
import           Control.Monad.Logger
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe
import qualified Data.Text                     as T
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
    (forever $ do
      packets <- receive 0 8
      liftIO $ do
        let pktsStr = map (T.pack . BC.unpack) packets
        mapM_ putStrLn pktsStr
        putStrLn (show packets :: Text)
        usleep 1000000
    )
