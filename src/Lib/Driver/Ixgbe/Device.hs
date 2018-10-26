-- |
-- Module      :  Lib.Driver.Ixgbe.Device
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description  
--

module Lib.Driver.Ixgbe.Device
  ( Device(..)
  , LinkSpeed(..)
  )
where

import           Lib.Driver.Ixgbe.Queue
import           Lib.Pci
import           Lib.Prelude

import qualified Data.Vector                   as V

data Device = Device { devBase :: Ptr Word32
                     , devBdf :: BusDeviceFunction
                     , devRxQueues :: V.Vector RxQueue
                     , devTxQueues :: V.Vector TxQueue
                     }

data LinkSpeed = LinkNotReady | Link100M | Link1G | Link10G
