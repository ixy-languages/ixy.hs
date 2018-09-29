{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Protolude

import Lib (Dev(..), Env(..), busDeviceFunction, init)

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Maybe (fromJust)
import Foreign.Ptr (nullPtr)
import System.Log.FastLogger (LogType(LogStdout), TimeFormat, newTimedFastLogger)
import System.Log.FastLogger.Date (newTimeCache)

newtype App a = App
    { runApp :: ReaderT Env IO a
    } deriving (Monad, Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadReader Env)

main :: IO ()
main = do
    tc <- newTimeCache ("[%Y-%m-%d %H:%M:%S]" :: TimeFormat)
    (logger, cleanup) <- newTimedFastLogger tc (LogStdout 4096)
    let env = Env {envLogger = (logger, cleanup)}
    -- runStateT (runReaderT (runApp run) env) dev
    runReaderT (runApp run) env
    cleanup

run :: App ()
run = do
    let bdf = fromJust $ busDeviceFunction "0000:02:00.0"
    let dev = Dev {devBase = nullPtr, devBdf = bdf, devNumTx = 0, devNumRx = 0, devRxQueues = [], devTxQueues = []}
     in evalStateT (init 512 512) dev
