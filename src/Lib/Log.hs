{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib.Log
    ( logLn
    , abort
    , halt
    , Logger
    ) where

import Lib.Prelude

import qualified Data.Text as T
import System.Log.FastLogger (LogStr, TimedFastLogger, ToLogStr, toLogStr)

type Logger = MonadReader TimedFastLogger

logLn :: (MonadIO m, MonadReader TimedFastLogger m) => Text -> m ()
logLn msg = do
    logger <- ask
    liftIO $ logger $ prepare msg

abort :: (Exception e, MonadIO m, Logger m) => e -> Text -> m a
abort e msg = do
    logLn $ "Exception caught: " <> T.pack (displayException e)
    logLn $ "Additional information: " <> msg
    liftIO exitFailure

halt :: (Exception e, MonadIO m, Logger m) => Text -> e -> m a
halt msg e = abort e msg

prepare :: ToLogStr msg => Text -> (msg -> LogStr)
prepare s f = toLogStr f <> " " <> toLogStr s <> "\n"
