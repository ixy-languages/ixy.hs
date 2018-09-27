{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Lib (Env)
import Lib.Pci
import Lib.Prelude

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Maybe (fromJust)
import System.Log.FastLogger (LogType(LogStdout), TimeFormat, TimedFastLogger, newTimedFastLogger)
import System.Log.FastLogger.Date (newTimeCache)

newtype App a = App
    { runApp :: ReaderT Env IO a
    } deriving (Monad, Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadReader Env)

main :: IO ()
main = do
    putStrLn ("Started." :: Text)
    tc <- newTimeCache ("[%Y-%m-%d %H:%M:%S]" :: TimeFormat)
    logger <- newTimedFastLogger tc (LogStdout 4096)
    let env = Env {envLogger=logger, envDevice=}
    runReaderT (runApp run) (logger, cleanup)
    cleanup

run :: App ()
run = do
    return ()
