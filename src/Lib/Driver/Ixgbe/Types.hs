{-# LANGUAGE TemplateHaskell #-}
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

module Lib.Driver.Ixgbe.Types
  ( Device(..)
  , devBase
  , devBdf
  , devRxQueues
  , devTxQueues
  , RxQueue(..)
  , rxqDescriptors
  , rxqBuffers
  , TxQueue(..)
  , txqDescriptors
  , txqBuffers
  , txqCleanNum
  , ReceiveDescriptor(..)
  , TransmitDescriptor(..)
  , LinkSpeed(..)
  )
where

import           Lib.Prelude
import           Lib.Pci

import           Control.Lens
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as Storable
import           Foreign.Ptr                    ( castPtr )
import           Foreign.Storable               ( sizeOf
                                                , alignment
                                                , peek
                                                , peekByteOff
                                                , poke
                                                , pokeByteOff
                                                )

-- $ IXGBE Advanced Descriptors

data ReceiveDescriptor
  = ReadRx { rdPacketAddr :: Word64
           , rdHeaderAddr :: Word64 }
  | WritebackRx { rdStatusError :: Word32
                , rdLength :: Word16 } deriving (Show)

instance Storable ReceiveDescriptor where
  sizeOf _ = 2 * sizeOf (0 :: Word64)
  alignment _ = sizeOf (0 :: Word64)
  peek ptr = do
    statusError <- peekByteOff ptr $ sizeOf (0 :: Word64)
    len <- peekByteOff ptr $ sizeOf (0 :: Word64) + sizeOf (0 :: Word32)
    return WritebackRx {rdStatusError=statusError, rdLength=len}
  poke ptr rdesc = do
    poke (castPtr ptr) $ rdPacketAddr rdesc
    pokeByteOff ptr (sizeOf (0 :: Word64)) $ rdHeaderAddr rdesc

data TransmitDescriptor
  = ReadTx { tdBufAddr :: Word64
           , tdCmdTypeLen :: Word32
           , tdOlInfoStatus :: Word32 }
  | WritebackTx { tdStatus :: Word32 }

instance Storable TransmitDescriptor where
  sizeOf _ = sizeOf (0 :: Word64) + sizeOf (0 :: Word32) + sizeOf (0 :: Word32)
  alignment = sizeOf
  peek ptr = do
    status <- peekByteOff ptr $ sizeOf (0 :: Word64) + sizeOf (0 :: Word32)
    return WritebackTx {tdStatus = status}
  poke ptr tdesc = do
    poke (castPtr ptr) $ tdBufAddr tdesc
    pokeByteOff ptr (sizeOf (0 :: Word64)) $ tdCmdTypeLen tdesc
    pokeByteOff ptr (sizeOf (0 :: Word64) + sizeOf (0 :: Word32)) $ tdOlInfoStatus tdesc

data RxQueue = RxQueue { _rxqDescriptors :: Storable.Vector (Ptr ReceiveDescriptor)
                       , _rxqBuffers :: Storable.Vector (Ptr Word8)}


makeLenses ''RxQueue

data TxQueue = TxQueue { _txqDescriptors :: Storable.Vector (Ptr TransmitDescriptor)
                       , _txqBuffers :: Storable.Vector (Ptr Word8)
                       , _txqCleanNum :: Int}

makeLenses ''TxQueue

data Device = Device { _devBase :: Ptr Word32
                     , _devBdf :: BusDeviceFunction
                     , _devRxQueues :: V.Vector RxQueue
                     , _devTxQueues :: V.Vector TxQueue
                     }

makeLenses ''Device

data LinkSpeed = LinkNotReady | Link100M | Link1G | Link10G
