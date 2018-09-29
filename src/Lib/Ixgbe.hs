{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib.Ixgbe
    ( init
    ) where

import qualified Lib.Ixgbe.Register as R
import Lib.Ixgbe.Types
    ( Dev(..)
    , DeviceState
    , LinkSpeed(..)
    , ReceiveDescriptor(..)
    , RxQueue(..)
    , Stats(..)
    , TransmitDescriptor(..)
    , TxQueue(..)
    )
import Lib.Log (Logger(..), halt, logLn)
import Lib.Memory (allocateDMA, allocateMemPool, allocatePktBuf)
import Lib.Memory.Types (MemPool(..), PacketBuf(..), Translation(..))
import Lib.Pci (mapResource)
import Lib.Pci.Types (BusDeviceFunction(..))
import Lib.Prelude

import Control.Monad.Catch (MonadCatch, catchAll)
import Data.Bits ((.&.), (.|.), complement)
import Data.List ((!!))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, peekByteOff, pokeByteOff, sizeOf)
import System.Posix.Unistd (usleep)

maxRxQueueEntries :: Int
maxRxQueueEntries = 4096

maxTxQueueEntries :: Int
maxTxQueueEntries = 4096

numRxQueueEntries :: Word
numRxQueueEntries = 512

numTxQueueEntries :: Word
numTxQueueEntries = 512

init :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Word -> Word -> m ()
init numRx numTx = do
    dev <- get
    ptr <- mapResource "resource0"
    put dev {devBase = ptr, devNumTx = numTx, devNumRx = numRx, devRxQueues = [], devTxQueues = []}
    catchAll resetAndInit handler
  where
    resetAndInit :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => m ()
    resetAndInit = do
        dev <- get
        let bdf = unBusDeviceFunction $ devBdf dev
        logLn $ "Resetting device " <> bdf <> "."
        reset
        logLn $ "Initializing device " <> bdf <> "."
        R.waitSet R.EEC autoReadDone
        R.waitSet R.RDRXCTL dmaInitCycleDone
        logLn $ "Initializing link for device " <> bdf <> "."
        initLink
        _ <- readStats
        initRx
        initTx
        forM_ [0 .. (fromIntegral (devNumRx dev) - 1)] startRxQueue
        forM_ [0 .. (fromIntegral (devNumTx dev) - 1)] startTxQueue
        setPromiscous
        waitForLink 1000
      where
        autoReadDone = 0x00000200
        dmaInitCycleDone = 0x00000008
        reset = do
            R.set R.EIMC disableInterrupt
            R.set R.CTRL ctrlRstMask
            R.waitClear R.CTRL ctrlRstMask
            R.set R.EIMC disableInterrupt
          where
            disableInterrupt = 0x7FFFFFFF
            ctrlRstMask = 0x04000008
        initLink = R.setMask R.AUTOC anRestart
          where
            anRestart = 0x00001000
    handler = halt "Error during setup of the IXGBE device."

readStats :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => m Stats
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

initRx :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m, MonadState Dev m) => m ()
initRx
    -- Disable RX while configuring.
 = do
    R.clearMask R.RXCTRL rxEnable
    -- Enable a single 128KB packet buffer.
    R.set (R.RXPBSIZE 0) bufferSize
    forM_ [1 .. 8] (\i -> R.set (R.RXPBSIZE i) 0)
    -- Enable CRC offloading.
    R.setMask R.HLREG0 crcStrip
    R.setMask R.RDRXCTL crcStrip
    -- Accept broadcast packets.
    R.setMask R.FCTRL broadcastAcceptMode
    -- Initialize queues
    dev <- get
    let numRx = fromIntegral (devNumRx dev)
     in do rxQueues <- forM [0 .. (numRx - 1)] setupQueue
           put (dev {devRxQueues = rxQueues})
           -- Disable weird flags.
           R.setMask R.CTRL_EXT noSnoopDisable
           forM_ [0 .. (numRx - 1)] (\i -> R.clearMask (R.DCA_RXCTRL i) (shift 1 12))
           -- Enable RX.
           R.setMask R.RXCTRL rxEnable
  where
    rxEnable = 0x00000001 -- Enable RX.
    bufferSize = 0x00020000 -- 128KB.
    crcStrip = 0x00000002
    broadcastAcceptMode = 0x00000400
    noSnoopDisable = 0x00010000
    setupQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m RxQueue
    setupQueue i = do
        logLn $ "Initializing RX queue " <> show i <> "."
        -- Enable advanced Rx descriptors.
        advRxDescEnable <- fmap (.|. (0x02000000 :: Word32)) ((.&. (0xF1FFFFFF :: Word32)) <$> R.get (R.SRRCTL i))
        R.set (R.SRRCTL i) $ fromIntegral advRxDescEnable
        -- Enable dropping of packets if no rx descriptors are available.
        R.setMask (R.SRRCTL i) dropEnable
        -- Setup descriptor ring.
        let ringSize = numRxQueueEntries * fromIntegral (sizeOf (undefined :: ReceiveDescriptor))
        t <- allocateDMA ringSize True
        memSet (trVirtual t) (fromIntegral ringSize) 0xFF
        R.set (R.RDBAL i) $ fromIntegral (trPhysical t .&. 0xFFFFFFFF)
        R.set (R.RDBAH i) $ fromIntegral (shift (trPhysical t) 32)
        R.set (R.RDLEN i) $ fromIntegral ringSize
        logLn $ "Rx ring " <> show i <> " | Physical: " <> show (trPhysical t) <> " | Virtual: " <> show (trVirtual t)
        -- Set ring to empty at the start.
        R.set (R.RDH i) 0
        R.set (R.RDT i) 0
        return
            RxQueue
                { rxqNumEntries = numRxQueueEntries
                , rxqMemPool = MemPool {mpBase = nullPtr, mpBufSize = 0, mpTop = 0} -- Dummy MemPool
                , rxqRxIndex = 0
                , rxqDescPtr = castPtr (trVirtual t) :: Ptr ReceiveDescriptor
                }
      where
        dropEnable = 0x10000000

startRxQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m ()
startRxQueue id = do
    logLn $ "Starting RX queue " <> show id <> "."
    dev <- get
    let queue = devRxQueues dev !! id
    memPool <- allocateMemPool (numRxQueueEntries + numTxQueueEntries) 2048
    logLn $ "Allocating " <> show (rxqNumEntries queue) <> " packet buffers for RX queue " <> show id <> "."
    forM_ [0 .. (fromIntegral (rxqNumEntries queue) - 1)] (setupDescriptor queue memPool)
    -- Enable queue and wait if necessary.
    R.setMask (R.RXDCTL id) rxdCtlEnable
    R.waitSet (R.RXDCTL id) rxdCtlEnable
    -- Rx queue starts out full.
    R.set (R.RDH id) 0
    R.set (R.RDT id) $ fromIntegral (rxqNumEntries queue - 1)
    -- return queue {rxqMemPool = memPool}
    let (x, _:ys) = splitAt id $ devRxQueues dev
    put dev {devRxQueues = x ++ queue {rxqMemPool = memPool} : ys}
  where
    rxdCtlEnable = 0x02000000
    setupDescriptor rxq memPool index = do
        desc <- liftIO $ peekByteOff (rxqDescPtr rxq) (index * sizeOf (undefined :: ReceiveDescriptor))
        (packetBufPtr, _) <- evalStateT allocatePktBuf memPool
        pb <- liftIO $ peek packetBufPtr
        let desc' = desc {rdPacketAddr = fromIntegral (pbPhysical pb), rdHeaderAddr = 0}
         in liftIO $ pokeByteOff (rxqDescPtr rxq) (index * sizeOf (undefined :: ReceiveDescriptor)) desc'

initTx :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => m ()
initTx
    -- CRC Offload and small packet padding.
 = do
    R.setMask R.HLREG0 crcAndPadEnable
    -- Default buffer size allocations
    R.set (R.TXPBSIZE 0) packetBuffer
    forM_ [1 .. 8] (\i -> R.set (R.TXPBSIZE i) 0)
    -- Required when not using DCB/VTd
    R.set R.DTXMXSZRQ 0xFFFF
    R.clearMask R.RTTDCS dcbArbiterDisable
    dev <- get
    let numTx = fromIntegral (devNumTx dev)
     in do txQueues <- forM [0 .. (numTx - 1)] setupQueue
           put dev {devTxQueues = txQueues}
           -- Enable DMA.
           R.set R.DMATXCTL transmitEnable
  where
    crcAndPadEnable = 0x00000401
    packetBuffer = 0x0000A000 -- 40KB
    dcbArbiterDisable = 0x00000040
    transmitEnable = 0x00000001
    setupQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m TxQueue
    setupQueue i = do
        logLn $ "Initializing TX queue " <> show i <> "."
        -- Setup descriptor ring.
        let ringSize = numTxQueueEntries * fromIntegral (sizeOf (undefined :: TransmitDescriptor))
        t <- allocateDMA ringSize True
        memSet (trVirtual t) (fromIntegral ringSize) 0xFF
        R.set (R.TDBAL i) $ fromIntegral (trPhysical t .&. 0xFFFFFFFF)
        R.set (R.TDBAH i) $ fromIntegral (trPhysical t `shift` 32)
        R.set (R.TDLEN i) $ fromIntegral ringSize
        logLn $ "Tx ring " <> show i <> " | Physical: " <> show (trPhysical t) <> " | Virtual: " <> show (trVirtual t)
        -- Descriptor writeback magic values.
        -- txdCtl <-
        --     fmap (.|. (36 .|. shift 8 8 .|. shift 4 16) . (.&. (complement $ shift 0x3F 16 .|. shift 0x3F 8 .|. 0x3F))) $ R.get (R.TXDCTL i)
        txdCtl <- wbMagic <$> R.get (R.TXDCTL i)
        R.set (R.TXDCTL i) txdCtl
        return
            TxQueue
                { txqNumEntries = numTxQueueEntries
                , txqTxIndex = 0
                , txqCleanIndex = 0
                , txqDescPtr = castPtr (trVirtual t) :: Ptr TransmitDescriptor
                }
      where
        wbMagic = (.|. 0x40824) . (.&. complement 0x3F3F3F)

startTxQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m ()
startTxQueue id = do
    logLn $ "Starting TX queue " <> show id <> "."
        -- Tx queue starts empty.
    R.set (R.TDH id) 0
    R.set (R.TDT id) 0
        -- Enable queue and wait if necessary.
    R.setMask (R.TXDCTL id) txdCtlEnable
    R.waitSet (R.TXDCTL id) txdCtlEnable
  where
    txdCtlEnable = 0x02000000

setPromiscous :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => m ()
setPromiscous = do
    dev <- get
    let bdf = unBusDeviceFunction $ devBdf dev
    logLn $ "Enabling promiscous mode for device " <> bdf <> "."
    R.set R.FCTRL (unicastPromiscousEnable .|. multicastPromiscousEnable)
  where
    multicastPromiscousEnable = 0x00000100
    unicastPromiscousEnable = 0x00000200

waitForLink :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m ()
waitForLink maxTries = do
    logLn "Waiting for link..."
    waitUntil 0
  where
    waitUntil numTries
        | numTries == maxTries = logLn "Maximum wait time for link exceeded."
    waitUntil numTries = do
        speed <- linkSpeed
        case speed of
            NotReady -> do
                liftIO $ usleep 10000
                waitUntil (numTries + 1)
            Speed100M -> logLn "Link speed was set to 100MBit/s"
            Speed1G -> logLn "Link speed was set to 1GBit/s"
            Speed10G -> logLn "Link speed was set to 10GBit/s"

linkSpeed :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => m LinkSpeed
linkSpeed = do
    links <- R.get R.LINKS
    if (links .&. linksUp) == 0
        then return NotReady
        else return $
             let l = links .&. links82599
              in case l
                     -- links100M -> Speed100M
                     -- links1G -> Speed1G
                     -- links10G -> Speed10G
                       of
                     _
                         | l == links100M -> Speed100M
                     _
                         | l == links1G -> Speed1G
                     _
                         | l == links10G -> Speed10G
                     _ -> NotReady
  where
    linksUp = 0x40000000
    links82599 = 0x30000000
    links100M = 0x10000000
    links1G = 0x20000000
    links10G = 0x30000000

-- receive :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m [PacketBuf]
-- receive queueId = do
--     dev <- get
--     let queue = devRxQueues dev !! queueId
memSet :: (MonadIO m) => Ptr Word -> Int -> Word8 -> m ()
memSet ptr size value = liftIO $ forM_ [0 .. (size - 1)] (\i -> pokeByteOff ptr i value)
