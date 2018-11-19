-- |
-- Module      :  Lib.Ixgbe.Queue
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--

module Lib.Ixgbe.Queue
  ( RxQueue(..)
  , TxQueue(..)
  , ReceiveDescriptor(..)
  , TransmitDescriptor(..)
  , mkRxQueue
  , mkTxQueue
  , numRxQueueEntries
  , numTxQueueEntries
  , nullReceiveDescriptor
  , isDone
  , isEndOfPacket
  , nullTransmitDescriptor
  , bufferSize
  )
where

import           Lib.Memory
import           Lib.Prelude

import           Data.IORef
import           Foreign.Ptr                    ( castPtr
                                                , plusPtr
                                                )
import           Foreign.Storable               ( sizeOf
                                                , alignment
                                                , peek
                                                , poke
                                                , peekByteOff
                                                , pokeByteOff
                                                )

numRxQueueEntries :: Int
numRxQueueEntries = 512

numTxQueueEntries :: Int
numTxQueueEntries = 512

bufferSize :: Int
bufferSize = 2048

data RxQueue = RxQueue { rxqDesc :: !(Int -> Ptr ReceiveDescriptor)
                       , rxqBuf :: !(Int -> Ptr Word8)
                       , rxqBufPhys :: !(Int -> Word64)
                       , rxqIndexRef :: !(IORef Int) }

mkRxQueue :: MonadIO m => Ptr ReceiveDescriptor -> Ptr Word8 -> Int -> m RxQueue
mkRxQueue descPtr bufPtr num = do
  let descriptor i = descPtr `plusPtr` (i * sizeOf nullReceiveDescriptor)
      buffer i = bufPtr `plusPtr` (i * bufferSize)
      PhysAddr bufPhysBase = translate $ VirtAddr bufPtr
      bufferPhys i = bufPhysBase + fromIntegral (i * bufferSize)
      indices      = [0 .. num - 1]
      descriptors  = map descriptor indices
      bufPhysAddrs = map (translate . VirtAddr . buffer) indices
  mapM_ writeDescriptor $ zip descriptors bufPhysAddrs
  indexRef <- liftIO $ newIORef (0 :: Int)
  return $! RxQueue
    { rxqDesc     = descriptor
    , rxqBuf      = buffer
    , rxqBufPhys  = bufferPhys
    , rxqIndexRef = indexRef
    }
 where
  writeDescriptor (ptr, PhysAddr bufPhysAddr) = liftIO
    $ poke ptr ReceiveRead {rdBufPhysAddr = bufPhysAddr, rdHeaderAddr = 0}

data TxQueue = TxQueue { txqDesc :: !(Int -> Ptr TransmitDescriptor)
                       , txqBuf :: !(Int -> Ptr Word8)
                       , txqBufPhys :: !(Int -> Word64)
                       , txqIndexRef :: IORef Int
                       , txqCleanRef :: IORef Int}

mkTxQueue :: MonadIO m => Ptr TransmitDescriptor -> Ptr Word8 -> m TxQueue
mkTxQueue descPtr bufPtr = do
  let descriptor i = descPtr `plusPtr` (i * sizeOf nullReceiveDescriptor)
      buffer i = bufPtr `plusPtr` (i * bufferSize)
      PhysAddr bufPhysBase = translate $ VirtAddr bufPtr
      bufferPhys i = bufPhysBase + fromIntegral (i * bufferSize)
  indexRef <- liftIO $ newIORef (0 :: Int)
  cleanRef <- liftIO $ newIORef (0 :: Int)
  return $! TxQueue
    { txqDesc     = descriptor
    , txqBuf      = buffer
    , txqBufPhys  = bufferPhys
    , txqIndexRef = indexRef
    , txqCleanRef = cleanRef
    }

data ReceiveDescriptor = ReceiveRead { rdBufPhysAddr :: !Word64
                                     , rdHeaderAddr :: !Word64 }
                         | ReceiveWriteback { rdStatus :: !Word32
                                            , rdLength :: !Word16}

instance Storable ReceiveDescriptor where
  sizeOf _ = 16
  alignment = sizeOf
  peek ptr = do
    status <- peekByteOff ptr 8
    len <- peekByteOff ptr 12
    return ReceiveWriteback {rdStatus=status, rdLength=len}
  poke ptr (ReceiveRead bufPhysAddr headerAddr) = do
    poke (castPtr ptr) bufPhysAddr
    pokeByteOff ptr 8 headerAddr
  poke _ (ReceiveWriteback _ _) = return $ panic "Cannot poke a writeback descriptor."

nullReceiveDescriptor :: ReceiveDescriptor
nullReceiveDescriptor = ReceiveRead {rdBufPhysAddr = 0, rdHeaderAddr = 0}

isDone :: ReceiveDescriptor -> Bool
isDone desc = testBit (rdStatus desc) 0

isEndOfPacket :: ReceiveDescriptor -> Bool
isEndOfPacket desc = testBit (rdStatus desc) 1

data TransmitDescriptor = TransmitRead { tdBufPhysAddr :: !Word64
                                       , tdCmdTypeLen :: !Word32
                                       , tdOlInfoStatus :: !Word32 }
                          | TransmitWriteback { tdStatus :: !Word32 }

instance Storable TransmitDescriptor where
  sizeOf _ = 16
  alignment = sizeOf
  peek ptr = do
    status <- peekByteOff ptr 12
    return TransmitWriteback {tdStatus = status}
  poke ptr (TransmitRead bufPhysAddr cmdTypeLen olInfoStatus) = do
    poke (castPtr ptr) bufPhysAddr
    pokeByteOff ptr 8 cmdTypeLen
    pokeByteOff ptr 12 olInfoStatus
  poke _ (TransmitWriteback _) = return $ panic "Cannot poke a writeback descriptor."

nullTransmitDescriptor :: TransmitDescriptor
nullTransmitDescriptor =
  TransmitRead {tdBufPhysAddr = 0, tdCmdTypeLen = 0, tdOlInfoStatus = 0}
