module Lib.Ixgbe.Types
    ( Dev(..)
    , Stats(..)
    , ReceiveDescriptor(..)
    , TransmitDescriptor(..)
    , RxQueue(..)
    , TxQueue(..)
    ) where

import Lib.Memory.Types (MemPool(..))
import Lib.Pci.Types (BusDeviceFunction)
import Lib.Prelude

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)

data Dev = Dev
    { devBase :: Ptr Word8
    , devBdf :: BusDeviceFunction
    , devNumRx :: Word
    , devNumTx :: Word
    , devRxQueues :: [RxQueue]
    , devTxQueues :: [TxQueue]
    }

data Stats = Stats
    { stRxPackets :: Word
    , stTxPackets :: Word
    , stRxBytes :: Word
    , stTxBytes :: Word
    }

data ReceiveDescriptor = AdvRecvDesc
    { rdBufAddr :: Word64
    , rdHeaderAddr :: Word64
    }

instance Storable ReceiveDescriptor where
    sizeOf rd = 2 * sizeOf (rdBufAddr rd :: Word64)
    alignment = sizeOf
    peek ptr = do
        addr <- peek (castPtr ptr :: Ptr Word)
        hdr <- peekByteOff (castPtr ptr :: Ptr Word) (sizeOf (undefined :: Word))
        return AdvRecvDesc {rdBufAddr = addr, rdHeaderAddr = hdr}
    poke ptr desc = do
        poke (castPtr ptr :: Ptr Word) (rdBufAddr desc)
        pokeByteOff (castPtr ptr :: Ptr Word) (sizeOf (undefined :: Word)) (rdHeaderAddr desc)

data TransmitDescriptor = AdvTransDesc
    { tdBufAddr :: Word64
    , tdCmdTypeLen :: Word32
    , tdOlInfoStatus :: Word32
    }

instance Storable TransmitDescriptor where
    sizeOf _ = sizeOf (0 :: Word) + sizeOf (0 :: Word32) + sizeOf (0 :: Word32)
    alignment = sizeOf
    peek ptr = do
        addr <- peek (castPtr ptr :: Ptr Word)
        cmdTypeLen <- peekByteOff (castPtr ptr :: Ptr Word32) (sizeOf (undefined :: Word))
        olInfoStatus <- peekByteOff (castPtr ptr :: Ptr Word32) (sizeOf (undefined :: Word) + sizeOf (undefined :: Word32))
        return AdvTransDesc {tdBufAddr = addr, tdCmdTypeLen = cmdTypeLen, tdOlInfoStatus = olInfoStatus}
    poke ptr desc = do
        poke (castPtr ptr :: Ptr Word) addr
        pokeByteOff (castPtr ptr :: Ptr Word32) (sizeOf addr) cmdTypeLen
        pokeByteOff (castPtr ptr :: Ptr Word32) (sizeOf addr + sizeOf cmdTypeLen) olInfoStatus
      where
        addr = tdBufAddr desc
        cmdTypeLen = tdCmdTypeLen desc
        olInfoStatus = tdOlInfoStatus desc

data RxQueue = RxQueue
    { rxqDesc :: ReceiveDescriptor
    , rxqNumEntries :: Word
    , rxqMemPool :: MemPool
    , rxqRxIndex :: Int
    }

instance Storable RxQueue where
    sizeOf _ = sizeOf (undefined :: ReceiveDescriptor) + sizeOf (0 :: Int) + sizeOf (undefined :: MemPool) + sizeOf (0 :: Int)
    alignment = sizeOf
    peek ptr = do
        desc <- peek (castPtr ptr :: Ptr ReceiveDescriptor)
        numEntries <- peekByteOff (castPtr ptr :: Ptr Int) (sizeOf desc)
        memPool <- peekByteOff (castPtr ptr :: Ptr MemPool) (sizeOf desc + sizeOf numEntries)
        rxIndex <- peekByteOff (castPtr ptr :: Ptr Int) (sizeOf desc + sizeOf numEntries + sizeOf memPool)
        return RxQueue {rxqDesc = desc, rxqNumEntries = numEntries, rxqMemPool = memPool, rxqRxIndex = rxIndex}
    poke ptr rxq = do
        poke (castPtr ptr :: Ptr ReceiveDescriptor) desc
        pokeByteOff (castPtr ptr :: Ptr Int) (sizeOf desc) numEntries
        pokeByteOff (castPtr ptr :: Ptr MemPool) (sizeOf desc + sizeOf numEntries) memPool
        pokeByteOff (castPtr ptr :: Ptr Int) (sizeOf desc + sizeOf numEntries + sizeOf memPool) rxIndex
      where
        desc = rxqDesc rxq
        numEntries = rxqNumEntries rxq
        memPool = rxqMemPool rxq
        rxIndex = rxqRxIndex rxq

data TxQueue = TxQueue
    { txqDesc :: TransmitDescriptor
    , txqNumEntries :: Word
    , txqCleanIndex :: Int
    , txqTxIndex :: Int
    }
