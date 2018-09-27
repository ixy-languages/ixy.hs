{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Protolude

import Lib (Dev(..), Env(..), busDeviceFunction, init)

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Maybe (fromJust)
import System.Log.FastLogger (LogType(LogStdout), TimeFormat, newTimedFastLogger)
import System.Log.FastLogger.Date (newTimeCache)

newtype App a = App
    { runApp :: ReaderT Env IO a
    } deriving (Monad, Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadReader Env)

main :: IO ()
main = do
    tc <- newTimeCache ("[%Y-%m-%d %H:%M:%S]" :: TimeFormat)
    (logger, cleanup) <- newTimedFastLogger tc (LogStdout 4096)
    let bdf = fromJust $ busDeviceFunction "0000:02:00.0"
    let env = Env {envLogger = (logger, cleanup), envDevice = Dev {devBdf = bdf}}
    runReaderT (runApp run) env
    cleanup

run :: App ()
run = do
    _ <- init 512 512
    return ()
