{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib.Driver.Ixgbe
    (
    ) where

import Lib.Driver.Ixgbe.Device (Device, IxgbeDevice, basePtr, numRx, numTx, pciBDF)
import qualified Lib.Driver.Ixgbe.Register as R
import Lib.Log (Logger, logLn)
import Lib.Memory (MemPool, allocateDMA, physical, virtual)
import Lib.Pci (BusDeviceFunction, mapResource, unBusDeviceFunction)
import Lib.Prelude

import Control.Monad.Catch (MonadCatch)
import Data.Bits ((.&.), (.|.), complement)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)

maxRxQueueEntries :: Int
maxRxQueueEntries = 4096

maxTxQueueEntries :: Int
maxTxQueueEntries = 4096

numRxQueueEntries :: Int
numRxQueueEntries = 512

numTxQueueEntries :: Int
numTxQueueEntries = 512

init :: (MonadIO m, Logger m) => BusDeviceFunction -> Int -> Int -> m IxgbeDevice
init bdf numRx numTx = do
    ptr <- mapResource bdf "resource0"
    return ()
  where
    resetAndInit :: (MonadIO m, Logger m, Device m) => m ()
    resetAndInit = do
        dev <- get
        let bdfT = unBusDeviceFunction (pciBDF dev)
        logLn $ "Resetting device " <> bdfT <> "."
        R.set R.EIMC disableInterrupt
        R.set R.CTRL ctrlRstMask
        R.waitClear R.CTRL ctrlRstMask
        R.set R.EIMC disableInterrupt
        logLn $ "Initializing device " <> bdfT <> "."
        initLink
        -- TODO: Read stats here.
      where
        disableInterrupt = 0x7FFFFFFF
        ctrlRstMask = 0x04000008
        initLink :: (MonadIO m, Logger m, Device m) => m ()
        initLink
            -- If this doesn't work look at ixgbe.c
         = do
            R.setMask R.AUTOC anRestart
          where
            anRestart = 0x00001000

data ReceiveDescriptor = AdvRecvDesc
    { bufAddr :: Word
    , headerAddr :: Word
    }

instance Storable ReceiveDescriptor where
    sizeOf _ = 2 * sizeOf (undefined :: Word)
    alignment = sizeOf
    peek ptr = do
        addr <- peek (castPtr ptr :: Ptr Word)
        hdr <- peekByteOff (castPtr ptr :: Ptr Word) (sizeOf (undefined :: Word))
        return AdvRecvDesc {bufAddr = addr, headerAddr = hdr}
    poke ptr desc = do
        poke (castPtr ptr :: Ptr Word) (bufAddr desc)
        pokeByteOff (castPtr ptr :: Ptr Word) (sizeOf (undefined :: Word)) (headerAddr desc)

initRx :: (MonadCatch m, MonadIO m, Logger m, Device m) => m ()
initRx = do
    R.clearMask R.RXCTRL rxEnable
    R.set (R.RXPBSIZE 0) bufferSize
    forM_ [1 .. 8] (\i -> R.set (R.RXPBSIZE i) 0)
    -- Enable CRC offloading
    R.setMask R.HLREG0 crcStrip
    R.setMask R.RDRXCTL crcStrip
    -- Accept broadcast packets
    R.setMask R.FCTRL broadcastAcceptMode
    -- Initialize queues
    dev <- get
    forM_ [0 .. (numRx dev)] initQueue
    R.setMask R.CTRL_EXT noSnoopDisable
    -- Disable weird flag
    forM_ [0 .. (numRx dev)] (\i -> R.clearMask (R.DCA_RXCTRL i) (shift 1 12))
    R.setMask R.RXCTRL rxEnable
  where
    rxEnable = 0x00000001
    bufferSize = 0x00020000 -- 128KB
    crcStrip = 0x00000002
    broadcastAcceptMode = 0x00000400
    noSnoopDisable = 0x00010000
    initQueue :: (MonadCatch m, MonadIO m, Logger m, Device m) => Int -> m ()
    initQueue i = do
        logLn $ "Initializing RX queue " <> show i <> "."
        current <- R.get (R.SRRCTL i)
        let advRxDescEnable = (current .&. 0xF1FFFFFF) .|. 0x02000000
        R.set (R.SRRCTL i) advRxDescEnable
        R.setMask (R.SRRCTL i) dropEnable
        let ringSize = numRxQueueEntries * sizeOf (undefined :: ReceiveDescriptor)
        t <- allocateDMA (fromIntegral ringSize) True
        initMem (virtual t) ringSize
        R.set (R.RDBAL i) (physical t .&. 0xFFFFFFFF)
        R.set (R.RDBAH i) (shift (physical t) 32)
        R.set (R.RDLEN i) (fromIntegral ringSize)
        logLn $ "Rx ring " <> show i <> " | Physical: " <> show (physical t) <> " | Virtual: " <> show (virtual t)
        -- Set ring to empty at the start
        R.set (R.RDH i) 0
        R.set (R.RDT i) 0
        -- TODO: Setup RxQueue here.
        return ()
      where
        dropEnable = 0x10000000
        initMem ptr size = liftIO $ forM_ [0 .. size] (\i -> pokeByteOff ptr i (0 :: Word))

initTx :: (MonadCatch m, MonadIO m, Logger m, Device m) => m ()
initTx
    -- CRC Offload and small packet padding
 = do
    R.setMask R.HLREG0 crcAndPadEnable
    -- Default buffer size allocations
    R.set (R.TXPBSIZE 0) packetBuffer
    forM_ [1 .. 8] (\i -> R.set (R.TXPBSIZE i) 0)
    R.set R.DTXMXSZRQ 0xFFFF
    R.clearMask R.RTTDCS dcbArbiterDisable
  where
    crcAndPadEnable = 0x00000401
    packetBuffer = 0x0000A000 -- 40KB
    dcbArbiterDisable = 0x00000040
