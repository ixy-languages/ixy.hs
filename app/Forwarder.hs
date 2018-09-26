{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib.Pci
import Protolude

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Maybe (fromJust)
import System.Log.FastLogger (LogType(LogStdout), TimeFormat, TimedFastLogger, newTimedFastLogger)
import System.Log.FastLogger.Date (newTimeCache)

newtype App a = App
    { runApp :: ReaderT (TimedFastLogger, IO ()) IO a
    } deriving (Monad, Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadReader (TimedFastLogger, IO ()))

main :: IO ()
main = do
    putStrLn ("Started." :: Text)
    tc <- newTimeCache ("[%Y-%m-%d %H:%M:%S]" :: TimeFormat)
    (logger, cleanup) <- newTimedFastLogger tc (LogStdout 4096)
    runReaderT (runApp run) (logger, cleanup)
    cleanup

run :: App ()
run = do
    return ()
