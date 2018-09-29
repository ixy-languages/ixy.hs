{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib.Ixgbe.Types
    ( Dev(..)
    , DeviceState
    , Stats(..)
    , ReceiveDescriptor(..)
    , TransmitDescriptor(..)
    , RxQueue(..)
    , TxQueue(..)
    , LinkSpeed(..)
    ) where

import Lib.Memory.Types (MemPool(..), PacketBuf(..))
import Lib.Pci.Types (BusDeviceFunction)
import Lib.Prelude

import Data.CircularList (CList)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)

type DeviceState = MonadState Dev

data Dev = Dev
    { devBase :: Ptr Word8
    , devBdf :: BusDeviceFunction
    , devNumRx :: Word
    , devNumTx :: Word
    , devRxQueues :: [RxQueue]
    , devTxQueues :: [TxQueue]
    } deriving (Show)

data RxQueue = RxQueue
    { rxqDescPtrs :: CList (Ptr ReceiveDescriptor, Ptr PacketBuf)
    , rxqNumEntries :: Word
    , rxqMemPool :: MemPool
    } deriving (Show)

data TxQueue = TxQueue
    { txqDescPtrs :: CList (Ptr TransmitDescriptor, Ptr PacketBuf)
    , txqNumEntries :: Word
    , txqCleanIndex :: Int
    } deriving (Show)

data Stats = Stats
    { stRxPackets :: Word
    , stTxPackets :: Word
    , stRxBytes :: Word
    , stTxBytes :: Word
    }

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

data LinkSpeed
    = NotReady
    | Speed100M
    | Speed1G
    | Speed10G
