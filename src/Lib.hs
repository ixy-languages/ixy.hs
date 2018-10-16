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
  ( newDriver
  , Driver(..)
  , Device(..)
  , devBdf
  , unBusDeviceFunction
  )
where

import           Lib.Driver
import           Lib.Driver.Ixgbe               ( )
import           Lib.Driver.Ixgbe.Types
import           Lib.Pci                        ( busDeviceFunction
                                                , unBusDeviceFunction
                                                )
import           Lib.Prelude

import           Control.Monad.Catch
import           Control.Monad.Logger           ( MonadLogger )

newDriver
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => Text
  -> Int
  -> Int
  -> m (Maybe Device)
newDriver bdfT numRx numTx = case busDeviceFunction bdfT of
  Just bdf -> do
    dev <- init bdf numRx numTx
    return $ Just dev
  Nothing -> return Nothing
