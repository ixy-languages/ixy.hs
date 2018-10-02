module Lib.Ixgbe
    ( init
    , receive
    ) where

import qualified Lib.Ixgbe.Register as R
import Lib.Ixgbe.Types
import Lib.Log (Logger(..), halt, logLn)
import Lib.Memory (allocateDMA, allocateMemPool, allocatePktBuf, allocatePktBufBatch)
import Lib.Memory.Types (MemPool(..), PacketBuf(..), Translation(..), bufAddr)
import Lib.Pci (mapResource)
import Lib.Pci.Types (BusDeviceFunction(..))
import Lib.Prelude

import Control.Lens hiding (element)
import Control.Lens.At (ix)
import Control.Monad.Catch (MonadCatch, catchAll)
import Data.Bits ((.&.), (.|.), complement)
import Data.CircularList (focus, fromList, rotR)
import Data.List ((!!))
import Data.Maybe (fromJust)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, pokeByteOff, sizeOf)
import System.IO.Error (userError)
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
init numRx numTx =
    if numRx > 0 && numRx <= 128
        then if numTx > 0 && numTx <= 128
                 then do
                     dev <- get
                     ptr <- mapResource "resource0"
                     put dev {devBase = ptr, devNumTx = numTx, devNumRx = numRx, _devRxQueues = [], _devTxQueues = []}
                     catchAll resetAndInit handler
                 else halt "Error during the initialization of the IXGBE driver." $
                      userError "Number of Tx queues must be between 1 and 128."
        else halt "Error during the initialization of the IXGBE driver." $ userError "Number of Rx queues must be between 1 and 128."
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
        R.dumpRegisters
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
    handler = halt "Error during the initialization of the IXGBE device."

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

initRx :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => m ()
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
    rxQueues <- forM [0 .. fromIntegral (devNumRx dev - 1)] setupQueue
    put $ dev & devRxQueues .~ rxQueues
    -- Disable weird flags.
    R.setMask R.CTRL_EXT noSnoopDisable
    forM_ [0 .. (devNumRx dev - 1)] (\i -> R.clearMask (R.DCA_RXCTRL $ fromIntegral i) (shift 1 12))
    -- Enable Rx.
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
        let ringSize = numRxQueueEntries * fromIntegral (sizeOf ReadRx {rdPacketAddr = 0, rdHeaderAddr = 0})
        t <- allocateDMA ringSize True
        memSet (trVirtual t) (fromIntegral ringSize) 0xFF
        R.set (R.RDBAL i) $ fromIntegral (trPhysical t .&. 0xFFFFFFFF)
        R.set (R.RDBAH i) $ fromIntegral (shift (trPhysical t) (-32))
        R.set (R.RDLEN i) $ fromIntegral ringSize
        logLn $ "Rx ring " <> show i <> " | Physical: " <> show (trPhysical t) <> " | Virtual: " <> show (trVirtual t)
        -- Set ring to empty at the start.
        R.set (R.RDH i) 0
        R.set (R.RDT i) 0
        -- Construct Rx queue.
        let base = castPtr (trVirtual t) :: Ptr ReceiveDescriptor
        let ptrs =
                [ (base `plusPtr` fromIntegral (j * fromIntegral (sizeOf ReadRx {rdPacketAddr = 0, rdHeaderAddr = 0})), nullPtr)
                | j <- [0 .. (numRxQueueEntries - 1)]
                ]
        return
            RxQueue
                { _rxqDescriptors = fromList ptrs
                , _rxqMemPool = MemPool {mpBase = nullPtr, mpBufSize = 0, mpTop = 0, mpFreeBufs = []}
                , rxqNumEntries = numRxQueueEntries
                , rxqIndex = 0
                }
      where
        dropEnable = 0x10000000

startRxQueue :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m ()
startRxQueue id = do
    logLn $ "Starting RX queue " <> show id <> "."
    dev <- get
    memPool <- allocateMemPool (fromIntegral (numRxQueueEntries + numTxQueueEntries)) 2048
    let queue = (dev ^. devRxQueues) !! id
    logLn $ "Allocating " <> show (rxqNumEntries queue) <> " packet buffers for RX queue " <> show id <> "."
    ((pbPtrs, _), memPool') <- runStateT (allocatePktBufBatch $ fromIntegral $ rxqNumEntries queue) memPool
    -- Update the used MemPool
    put $ dev & devRxQueues .~ ((dev ^. devRxQueues) & ix id .~ (queue & rxqMemPool .~ memPool'))
    -- Setup the descriptors.
    let ptrs = zip (toList (map fst (queue ^. rxqDescriptors))) pbPtrs
    forM_ ptrs setupDescriptor
    put $ dev & devRxQueues .~ ((dev ^. devRxQueues) & ix id .~ (queue & rxqDescriptors .~ fromList ptrs))
    -- Enable the rx queue and wait.
    R.setMask (R.RXDCTL id) rxdCtlEnable
    R.waitSet (R.RXDCTL id) rxdCtlEnable
    -- Set RX queue to full.
    R.set (R.RDH id) 0
    R.set (R.RDT id) $ fromIntegral (rxqNumEntries queue - 1)
  where
    setupDescriptor (descPtr, pbPtr) = do
        packetBuf <- liftIO $ peek pbPtr
        let desc = ReadRx {rdPacketAddr = bufAddr packetBuf, rdHeaderAddr = 0}
         in liftIO $ poke descPtr desc
    rxdCtlEnable = 0x2000000

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
    txQueues <- forM [0 .. fromIntegral (devNumTx dev - 1)] setupQueue
    put $ dev & devTxQueues .~ txQueues
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
        let ringSize = numTxQueueEntries * fromIntegral (sizeOf ReadTx {tdBufAddr = 0, tdCmdTypeLen = 0, tdOlInfoStatus = 0})
        t <- allocateDMA ringSize True
        memSet (trVirtual t) (fromIntegral ringSize) 0xFF
        R.set (R.TDBAL i) $ fromIntegral (trPhysical t .&. 0xFFFFFFFF)
        R.set (R.TDBAH i) $ fromIntegral (trPhysical t `shift` 32)
        R.set (R.TDLEN i) $ fromIntegral ringSize
        logLn $ "Tx ring " <> show i <> " | Physical: " <> show (trPhysical t) <> " | Virtual: " <> show (trVirtual t)
        txdCtl <- wbMagic <$> R.get (R.TXDCTL i)
        R.set (R.TXDCTL i) txdCtl
        let base = castPtr (trVirtual t) :: Ptr TransmitDescriptor
        let ptrs =
                [ (base `plusPtr` fromIntegral (j * fromIntegral (sizeOf ReadRx {rdPacketAddr = 0, rdHeaderAddr = 0})), nullPtr)
                | j <- [0 .. (numRxQueueEntries - 1)]
                ]
        return TxQueue {txqNumEntries = numTxQueueEntries, txqCleanIndex = 0, _txqDescriptors = fromList ptrs}
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
    R.setMask R.FCTRL (unicastPromiscousEnable .|. multicastPromiscousEnable)
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
            NoSpeed -> do
                liftIO $ usleep 10000
                waitUntil (numTries + 1)
            Speed100M -> logLn "Link speed was set to 100MBit/s"
            Speed1G -> logLn "Link speed was set to 1GBit/s"
            Speed10G -> logLn "Link speed was set to 10GBit/s"

linkSpeed :: (MonadIO m, MonadReader env m, Logger env, DeviceState m) => m LinkSpeed
linkSpeed = do
    links <- R.get R.LINKS
    if (links .&. linksUp) == 0
        then return NoSpeed
        else return $
             let l = links .&. links82599
              in case l of
                     _
                         | l == links100M -> Speed100M
                     _
                         | l == links1G -> Speed1G
                     _
                         | l == links10G -> Speed10G
                     _ -> NoSpeed
  where
    linksUp = 0x40000000
    links82599 = 0x30000000
    links100M = 0x10000000
    links1G = 0x20000000
    links10G = 0x30000000

receive :: (MonadCatch m, MonadIO m, MonadReader env m, Logger env, DeviceState m) => Int -> m [PacketBuf]
receive queueId = inner 0
  where
    inner passes = do
        dev <- get
        let queue = (dev ^. devRxQueues) !! queueId
        let (descPtr, pbPtr) = fromJust $ focus (queue ^. rxqDescriptors)
        descriptor <- liftIO $ peek descPtr
        h <- R.get (R.RDH queueId)
        logLn $ "Ring buffer head is at " <> show h <> "."
        -- logLn $ ("Ring buffer head is at " <> show) <$> R.get (R.RDH queueId)
        let status = rdStatusError descriptor
         in if isDone status
                then if not $ isEOP status
                         then halt "End of packet in rx descriptor was not set." $ userError "Multi-segment packets are not supported."
                         else do
                             packetBuf <- readPacket descriptor pbPtr
                             resetDescriptor descPtr
                             put $
                                 dev &
                                 devRxQueues .~
                                 ((dev ^. devRxQueues) & ix queueId .~ (queue & rxqDescriptors .~ rotR (queue ^. rxqDescriptors)))
                             (packetBuf :) <$> inner (passes + 1)
                else do
                    put $ dev & devRxQueues .~ ((dev ^. devRxQueues) & ix queueId .~ queue {rxqIndex = rxqIndex queue + passes})
                    R.set (R.RDT queueId) $ fromIntegral (rxqIndex queue + passes)
                    return []
    isDone s = s .&. 0x01 /= 0
    isEOP s = s .&. 0x02 /= 0
    readPacket desc pbPtr = do
        pb <- liftIO $ peek pbPtr
        return pb {pbBufSize = fromIntegral $ rdLength desc}
    resetDescriptor descPtr = do
        dev <- get
        let queue = (dev ^. devRxQueues) !! queueId
        ((pbPtr, _), memPool) <- runStateT allocatePktBuf (queue ^. rxqMemPool)
        put $ dev & devRxQueues .~ ((dev ^. devRxQueues) & ix queueId .~ (queue & rxqMemPool .~ memPool))
        pb <- liftIO $ peek pbPtr
        let desc = ReadRx {rdPacketAddr = bufAddr pb, rdHeaderAddr = 0}
         in liftIO $ poke descPtr desc

memSet :: (MonadIO m) => Ptr Word -> Int -> Word8 -> m ()
memSet ptr size value = liftIO $ forM_ [0 .. (size - 1)] (\i -> pokeByteOff ptr i value)
