{-# LANGUAGE BangPatterns #-}
-- |
-- Module      :  Lib
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--
module Lib
  ( -- * Driver
    newDriver
  , module Lib.Ixgbe
  )
where

import           Lib.Ixgbe
import           Lib.Pci                        ( busDeviceFunction )
import           Lib.Prelude

import           Control.Monad.Catch
import           Control.Monad.Logger           ( MonadLogger )

-- | Initializes a driver for a device.
--
-- Currently only supports IXGBE.
newDriver
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => Text -- ^ The 'BusDeviceFunction' of the device.
  -> Int -- ^ The number of rx queues to initialize.
  -> Int -- ^ The number of tx queues to initialize.
  -> m (Maybe Device)
newDriver bdfT numRx numTx = case busDeviceFunction bdfT of
  Just bdf -> do
    !dev <- init bdf numRx numTx
    return $ Just dev
  Nothing -> return Nothing
