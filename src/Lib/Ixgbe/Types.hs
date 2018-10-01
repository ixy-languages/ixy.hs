{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Ixgbe.Types where

import Lib.Memory.Types (MemPool(..), PacketBuf(..))
import Lib.Pci.Types (BusDeviceFunction)
import Lib.Prelude

import Control.Lens
import Data.CircularList (CList)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)

data ReceiveDescriptor
    = ReadRx { rdPacketAddr :: Word64
             , rdHeaderAddr :: Word64 }
    | WritebackRx { rdStatusError :: Word32
                  , rdLength :: Word16 }

instance Storable ReceiveDescriptor where
    sizeOf _ = 2 * sizeOf (0 :: Word64)
    alignment = sizeOf
    peek ptr = do
        statusError <- peekByteOff (castPtr ptr :: Ptr Word32) (sizeOf (0 :: Word64))
        len <- peekByteOff (castPtr ptr :: Ptr Word16) (sizeOf (0 :: Word64) + sizeOf (0 :: Word32))
        return WritebackRx {rdStatusError = statusError, rdLength = len}
    poke ptr rdesc = do
        poke (castPtr ptr :: Ptr Word64) $ rdPacketAddr rdesc
        pokeByteOff (castPtr ptr :: Ptr Word64) (sizeOf (0 :: Word64)) $ rdHeaderAddr rdesc

data TransmitDescriptor
    = ReadTx { tdBufAddr :: Word64
             , tdCmdTypeLen :: Word32
             , tdOlInfoStatus :: Word32 }
    | WritebackTx { tdStatus :: Word32 }

instance Storable TransmitDescriptor where
    sizeOf _ = sizeOf (0 :: Word64) + sizeOf (0 :: Word32) + sizeOf (0 :: Word32)
    alignment = sizeOf
    peek ptr = do
        status <- peekByteOff (castPtr ptr :: Ptr Word32) (sizeOf (0 :: Word64) + sizeOf (0 :: Word32))
        return WritebackTx {tdStatus = status}
    poke ptr tdesc = do
        poke (castPtr ptr :: Ptr Word64) $ tdBufAddr tdesc
        pokeByteOff (castPtr ptr :: Ptr Word32) (sizeOf (0 :: Word64)) $ tdCmdTypeLen tdesc
        pokeByteOff (castPtr ptr :: Ptr Word32) (sizeOf (0 :: Word64) + sizeOf (0 :: Word32)) $ tdOlInfoStatus tdesc

data RxQueue = RxQueue
    { _rxqDescriptors :: CList (Ptr ReceiveDescriptor, Ptr PacketBuf)
    , _rxqMemPool :: MemPool
    , rxqNumEntries :: Word
    } deriving (Show)

makeLenses ''RxQueue

data TxQueue = TxQueue
    { _txqDescriptors :: CList (Ptr TransmitDescriptor, Ptr PacketBuf)
    , txqNumEntries :: Word
    , txqCleanIndex :: Int
    } deriving (Show)

makeLenses ''TxQueue

type DeviceState = MonadState Device

data Device = Device
    { devBase :: Ptr Word32
    , devBdf :: BusDeviceFunction
    , devNumRx :: Word
    , devNumTx :: Word
    , _devRxQueues :: [RxQueue]
    , _devTxQueues :: [TxQueue]
    } deriving (Show)

makeLenses ''Device

data Stats = Stats
    { stRxPackets :: Word
    , stTxPackets :: Word
    , stRxBytes :: Word
    , stTxBytes :: Word
    } deriving (Show)

data LinkSpeed
    = NoSpeed
    | Speed100M
    | Speed1G
    | Speed10G
