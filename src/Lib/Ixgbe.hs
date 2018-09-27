{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib.Ixgbe
    (
    ) where

import Lib.Core (Env(..), LogType)
import qualified Lib.Ixgbe.Register as R
import Lib.Ixgbe.Types (Dev(..), ReceiveDescriptor(..), RxQueue(..), Stats(..), TransmitDescriptor(..), TxQueue(..))
import Lib.Ixgbe.Types.Extended (Device(..))
import Lib.Log (Logger, logLn)
import Lib.Memory (allocateDMA)
import Lib.Memory.Types (MemPool(..), Translation(..))
import Lib.Pci (mapResource)
import Lib.Pci.Types (BusDeviceFunction(..))
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

init :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, Device env, MonadState Dev m) => Int -> Int -> m ()
init numRx numTx = do
    env <- ask
    ptr <- mapResource "resource0"
    let bdf = devBdf $ getDevice env
     in put Dev {devBdf = bdf, devBase = ptr, devNumRx = numRx, devNumTx = numTx, devRxQueues = [], devTxQueues = []}

resetAndInit :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, Device env, MonadState Dev m) => m ()
resetAndInit = do
    env <- ask
    let bdf = unBusDeviceFunction $ devBdf $ getDevice env
    logLn $ "Resetting device " <> bdf <> "."
    R.set R.EIMC disableInterrupt
    R.set R.CTRL ctrlRstMask
    R.waitClear R.CTRL ctrlRstMask
    R.set R.EIMC disableInterrupt
    logLn $ "Initializing device " <> bdf <> "."
    initLink
    _ <- readStats
    (_, dev) <- runState initRx $ getDevice env
    return ()
  where
    disableInterrupt = 0x7FFFFFFF
    ctrlRstMask = 0x04000008
    initLink :: (MonadIO m, MonadReader env m, Logger env, Device env) => m ()
    initLink
         -- If this doesn't work look at ixgbe.c
     = R.setMask R.AUTOC anRestart
      where
        anRestart = 0x00001000

readStats :: (MonadIO m, MonadReader env m, Logger env, Device env) => m Stats
readStats = do
    rxPackets <- R.get R.GPRC
    txPackets <- R.get R.GPTC
    rxBytesL <- R.get R.GORCL
    rxBytesH <- (`shift` 32) <$> R.get R.GORCH
    txBytesL <- R.get R.GOTCL
    txBytesH <- (`shift` 32) <$> R.get R.GOTCH
    return
        Stats
            { stRxPackets = fromIntegral rxPackets
            , stTxPackets = fromIntegral txPackets
            , stRxBytes = fromIntegral rxBytesL + fromIntegral rxBytesH
            , stTxBytes = fromIntegral txBytesL + fromIntegral txBytesH
            }

initRx :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, Device env, MonadState Dev m) => m ()
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
    env <- ask
    let numRx = devNumRx $ getDevice env
    forM_ [0 .. numRx] initQueue
    R.setMask R.CTRL_EXT noSnoopDisable
    -- Disable weird flag
    forM_ [0 .. numRx] (\i -> R.clearMask (R.DCA_RXCTRL i) (shift 1 12))
    R.setMask R.RXCTRL rxEnable
  where
    rxEnable = 0x00000001
    bufferSize = 0x00020000 -- 128KB
    crcStrip = 0x00000002
    broadcastAcceptMode = 0x00000400
    noSnoopDisable = 0x00010000
    initQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, Device env, MonadState Dev m) => Int -> m ()
    initQueue i = do
        logLn $ "Initializing RX queue " <> show i <> "."
        advRxDescEnable <- fmap (.|. (0x02000000 :: Word32)) ((.&. (0xF1FFFFFF :: Word32)) <$> R.get (R.SRRCTL i))
        R.set (R.SRRCTL i) $ fromIntegral advRxDescEnable
        R.setMask (R.SRRCTL i) $ fromIntegral dropEnable
        let ringSize = numRxQueueEntries * sizeOf (undefined :: ReceiveDescriptor)
        t <- allocateDMA (fromIntegral ringSize) True
        memSet (trVirtual t) ringSize 0xFF
        R.set (R.RDBAL i) $ fromIntegral (trPhysical t .&. 0xFFFFFFFF)
        R.set (R.RDBAH i) $ fromIntegral (shift (trPhysical t) 32)
        R.set (R.RDLEN i) $ fromIntegral (fromIntegral ringSize)
        logLn $ "Rx ring " <> show i <> " | Physical: " <> show (trPhysical t) <> " | Virtual: " <> show (trVirtual t)
        -- Set ring to empty at the start
        R.set (R.RDH i) 0
        R.set (R.RDT i) 0
        dev <- get
        desc <- liftIO $ peek (castPtr (trVirtual t) :: Ptr ReceiveDescriptor)
        let dev' = dev {devRxQueues = (devRxQueues dev) ++ [RxQueue {rxqNumEntries = numRxQueueEntries, rxqRxIndex = 0, rxqDesc = desc}]}
         in put dev'
      where
        dropEnable = 0x10000000

startRxQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, Device env, MonadState Dev m) => Int -> m ()
startRxQueue index = do
    logLn $ "Starting RX queue " <> show index <> "."
    dev <- get
    let memPoolSize = numTxQueueEntries + numRxQueueEntries
    let queue = (devRxQueues dev) !! index
    let memPool = allocateMemPool memPoolSize 2048
    let queue' = queue {rxqMemPool = memPool}
    forM_ [0 .. (rxqNumEntries queue')]

initTx :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, Device env, MonadState Dev m) => m ()
initTx
    -- CRC Offload and small packet padding
 = do
    R.setMask R.HLREG0 crcAndPadEnable
    -- Default buffer size allocations
    R.set (R.TXPBSIZE 0) packetBuffer
    forM_ [1 .. 8] (\i -> R.set (R.TXPBSIZE i) 0)
    R.set R.DTXMXSZRQ 0xFFFF
    R.clearMask R.RTTDCS dcbArbiterDisable
    env <- ask
    let numTx = devNumTx $ getDevice env
    forM_ [0 .. numTx] initQueue
    R.set R.DMATXCTL transmitEnable
  where
    crcAndPadEnable = 0x00000401
    packetBuffer = 0x0000A000 -- 40KB
    dcbArbiterDisable = 0x00000040
    transmitEnable = 0x00000001
    initQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, Device env, MonadState Dev m) => Int -> m ()
    initQueue i = do
        logLn $ "Initializing TX queue " <> show i <> "."
        let ringSize = numTxQueueEntries * sizeOf (undefined :: TransmitDescriptor)
        t <- allocateDMA (fromIntegral ringSize) True
        memSet (trVirtual t) ringSize 0xFF
        R.set (R.TDBAL i) $ fromIntegral (trPhysical t .&. 0xFFFFFFFF)
        R.set (R.TDBAH i) $ fromIntegral (trPhysical t `shift` 32)
        R.set (R.TDLEN i) $ fromIntegral ringSize
        logLn $ "Tx ring " <> show i <> " | Physical: " <> show (trPhysical t) <> " | Virtual: " <> show (trVirtual t)
        txdCtl <-
            fmap
                (.|. (36 .|. (shift 8 8) .|. (shift 4 16)))
                (fmap (.&. complement (shift 0x3F 16 .|. shift 0x3F 8 .|. 0x3F)) (R.get (R.TXDCTL i)))
        R.set (R.TXDCTL i) txdCtl
        dev <- get
        desc <- liftIO $ peek (castPtr (trVirtual t) :: Ptr TransmitDescriptor)
        let dev' = dev {devTxQueues = TxQueue {txqNumEntries = numTxQueueEntries, txqDesc = desc} : (devTxQueues dev)}
         in put dev'

memSet :: (MonadIO m) => Ptr Word -> Int -> Word8 -> m ()
memSet ptr size value = liftIO $ forM_ [0 .. size] (\i -> pokeByteOff ptr i value)
