-- |
-- Module      :  Lib.Driver
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module Lib.Driver
  ( Driver(..)
  , Statistics(..)
  , SendResult(..)
  )
where

import           Lib.Prelude
import           Lib.Pci

import           Control.Monad.Catch
import           Control.Monad.Logger           ( MonadLogger )

class Driver a where
  init :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m) => BusDeviceFunction -> Int -> Int -> m a
  stats :: (MonadIO m, MonadReader a m) => m Statistics
  setPromiscuous :: (MonadIO m, MonadReader a m, MonadLogger m) => Bool -> m ()
  receive :: (MonadThrow m, MonadIO m, MonadState a m, MonadLogger m) => Int -> Int -> m [ByteString]
  send :: (MonadIO m, MonadState a m) => Int -> [ByteString] -> m SendResult

data SendResult = Done | Partial [ByteString]

data Statistics = Statistics { stRxPackets :: Int
                             , stTxPackets :: Int
                             , stRxBytes :: Int
                             , stTxBytes :: Int
                             }
