module Lib.Log
    ( Logger(..)
    , logLn
    , abort
    , halt
    ) where

import Lib.Core (Env(envLogger), LogType)
import Lib.Prelude

import qualified Data.Text as T
import System.Log.FastLogger (LogStr, ToLogStr, toLogStr)

class Logger a where
    getLogger :: a -> LogType

instance Logger Env where
    getLogger = envLogger

logLn :: (MonadIO m, MonadReader env m, Logger env) => Text -> m ()
logLn msg = do
    env <- ask
    liftIO $ fst (getLogger env) $ prepare msg

abort :: (Exception e, MonadIO m, MonadReader env m, Logger env) => e -> Text -> m a
abort e msg = do
    env <- ask
    logLn $ "Exception caught: " <> T.pack (displayException e)
    logLn $ "Additional information: " <> msg
    liftIO $ do
        snd $ getLogger env -- Execute the cleanup action of the logger.
        exitFailure

halt :: (Exception e, MonadIO m, MonadReader env m, Logger env) => Text -> e -> m a
halt msg e = abort e msg

prepare :: ToLogStr msg => Text -> (msg -> LogStr)
prepare s f = toLogStr f <> " " <> toLogStr s <> "\n"
