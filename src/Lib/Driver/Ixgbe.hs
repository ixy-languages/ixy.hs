{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Lib.Driver.Ixgbe
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--

module Lib.Driver.Ixgbe
  ( init
  )
where

import qualified Lib.Driver                    as Driver
import           Lib.Driver.Ixgbe.Device
import           Lib.Driver.Ixgbe.Descriptor
import qualified Lib.Driver.Ixgbe.Register     as R
import           Lib.Driver.Ixgbe.Queue
import           Lib.Memory
import           Lib.Pci
import           Lib.Prelude

import           Control.Exception              ( assert )
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.IORef
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Foreign.Marshal.Array          ( peekArray
                                                , pokeArray
                                                )
import           Foreign.Marshal.Utils          ( fillBytes )
import           Foreign.Storable               ( sizeOf
                                                , peek
                                                )
import           Numeric                        ( showHex )
import           System.IO.Error                ( userError )
import           System.Posix.Unistd            ( usleep )

txCleanBatch :: Int
txCleanBatch = 32

-- | Initializes a IXGBE NIC and returns a 'Driver'.
init
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => BusDeviceFunction -- ^ The 'BusDeviceFunction' of the IXGBE NIC.
  -> Int -- ^ The number of rx queues that will be initialized.
  -> Int -- ^ The number of tx queues that will be initialized.
  -> m Driver.Driver
init bdf numRx numTx = do
  $(logInfo) $ "Initializing device " <> unBusDeviceFunction bdf <> "."
  ptr      <- mapResource bdf "resource0"
  (_, dev) <- runStateT
    go
    Device
      { devBase     = ptr
      , devBdf      = bdf
      , devRxQueues = V.empty
      , devTxQueues = V.empty
      }
  return $! Driver.Driver
    { Driver.receive    = receive dev
    , Driver.send       = send dev
    , Driver.setPromisc = setPromisc dev
    , Driver.stats      = stats dev
    , Driver.dump       = dump dev
    }
 where
  go = do
    dev <- get
    runReaderT
      (do
        reset
        R.waitSet R.EEC autoReadDone
        R.waitSet R.RDRXCTL dmaInitCycleDone
        initLink
      )
      dev
    _        <- liftIO $ stats dev
    rxQueues <- runReaderT
      (do
        queues <- initRx numRx
        mapM startRxQueue $ zip queues [0 ..]
      )
      dev
    txQueues <- runReaderT
      (do
        queues <- initTx numTx
        mapM startTxQueue $ zip queues [0 ..]
      )
      dev
    put $ dev { devRxQueues = V.fromList rxQueues
              , devTxQueues = V.fromList txQueues
              }
    liftIO $ setPromisc dev True
    runReaderT (waitForLink 1000) dev
   where
    dmaInitCycleDone = 0x8
    autoReadDone     = 0x200
    reset            = do
      R.set R.EIMC disableInterrupt
      R.set R.CTRL resetMask
      R.waitClear R.CTRL resetMask
      R.set R.EIMC disableInterrupt
     where
      resetMask        = 0x4000008
      disableInterrupt = 0x7FFFFFFF
    initLink = do
      R.set R.AUTOC =<< lms <$> R.get R.AUTOC
      R.set R.AUTOC =<< magic <$> R.get R.AUTOC
      R.setMask R.AUTOC anRestart
     where
      anRestart = 0x1000
      lms       = (.|. shift 0x3 13) . (.&. complement (shift 0x7 13))
      magic     = (.|. (shift 0x0 7 :: Word32)) . (.&. complement 0x00000180)


  stats :: Device -> IO Driver.Stats
  stats = runReaderT
    (do
      rxPkts   <- R.get R.GPRC
      txPkts   <- R.get R.GPTC
      rxBytesL <- R.get R.GORCL
      rxBytesH <- (`shift` 32) <$> R.get R.GORCH
      txBytesL <- R.get R.GOTCL
      txBytesH <- (`shift` 32) <$> R.get R.GOTCH
      return Driver.Stats
        { Driver.stRxPkts  = fromIntegral rxPkts
        , Driver.stTxPkts  = fromIntegral txPkts
        , Driver.stRxBytes = fromIntegral (rxBytesL + rxBytesH)
        , Driver.stTxBytes = fromIntegral (txBytesL + txBytesH)
        }
    )

  setPromisc :: Device -> Bool -> IO ()
  setPromisc dev flag = runReaderT
    (if flag
      then R.setMask R.FCTRL promiscEnable
      else R.clearMask R.FCTRL promiscEnable
    )
    dev
    where promiscEnable = 0x300

  dump :: Device -> IO Text
  dump dev = do
    output <- runReaderT R.dumpRegisters dev
    return $ foldr (<>) "" output

-- | Initialize a number of rx queues.
initRx
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => Int -- ^ The amount of queues that will be initialized.
  -> m [Ptr ReceiveDescriptor] -- ^ The initialized queues.
initRx numRx = do
  deviceId <- showDeviceId
  $(logInfo) $ deviceId <> " Initializing " <> show numRx <> " rx queues."

  -- Disable Rx while configuring.
  R.clearMask R.RXCTRL rxEnable

  -- Set packet buffer sizes.
  R.set (R.RXPBSIZE 0) bufferSize
  forM_ [1 .. 7] (\i -> R.set (R.RXPBSIZE i) 0)

  -- Enable CRC offloading.
  R.setMask R.HLREG0 crcStrip
  R.setMask R.RDRXCTL crcStrip

  -- Enable accepting of broadcast packets.
  R.setMask R.FCTRL broadcastAcceptMode

  -- Do per-queue configuration.
  rxQueues <- mapM initQueue [0 .. (numRx - 1)]

  -- No snoop disable.
  R.setMask R.CTRL_EXT noSnoopDisable

  -- Magic flags for broken feature.
  forM_ [0 .. (numRx - 1)] (\i -> R.clearMask (R.DCA_RXCTRL i) $ shift 1 12)

  -- Enable Rx again.
  R.setMask R.RXCTRL rxEnable

  return rxQueues
 where
  rxEnable            = 0x1
  crcStrip            = 0x2
  broadcastAcceptMode = 0x400
  noSnoopDisable      = 0x10000
  bufferSize          = 0x20000
  initQueue
    :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
    => Int
    -> m (Ptr ReceiveDescriptor)
  initQueue id = do
    deviceId <- showDeviceId
    $(logDebug) $ deviceId <> " Initializing rx queue " <> show id <> "."

    -- Enable advanced receive descriptors.
    advRxDescEnable <- fmap
      (.|. (0x02000000 :: Word32))
      ((.&. (0xF1FFFFFF :: Word32)) <$> R.get (R.SRRCTL id))
    R.set (R.SRRCTL id) advRxDescEnable

    -- Enable dropping of packets, when all descriptors are full.
    R.setMask (R.SRRCTL id) dropEnable

    let size = numRxQueueEntries * sizeOf nullReceiveDescriptor
    descPtr  <- allocateDescriptors size

    -- Setup descriptor ring.
    physAddr <- translate descPtr
    R.set (R.RDBAL id) $ fromIntegral (physAddr .&. 0xFFFFFFFF)
    R.set (R.RDBAH id) $ fromIntegral (shift physAddr (-32))
    R.set (R.RDLEN id) $ fromIntegral size

    $(logDebug)
      $  deviceId
      <> " Rx ring "
      <> show id
      <> " at "
      <> show descPtr
      <> "(phy="
      <> T.pack (showHex physAddr "")
      <> ")."

    -- Set ring to empty at the start.
    R.set (R.RDH id) 0
    R.set (R.RDT id) 0

    return $! descPtr
    where dropEnable = 0x10000000
startRxQueue
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => (Ptr ReceiveDescriptor, Int)
  -> m RxQueue
startRxQueue (descPtr, id) = do
  deviceId <- showDeviceId
  $(logDebug) $ deviceId <> " Starting rx queue " <> show id <> "."

  -- Setup buffers for descriptors.
  -- Unlike the original implementation we identity map buffers to descriptors
  -- and do not change this anymore.
  let size = assert (numRxQueueEntries .&. (numRxQueueEntries - 1) == 0)
                    (numRxQueueEntries * 2048)
  bufPtr <- allocateRaw size True
  queue  <- mkRxQueueM descPtr bufPtr

  -- Enable queue and wait.
  R.setMask (R.RXDCTL id) rxdctlEnable
  R.waitSet (R.RXDCTL id) rxdctlEnable

  -- Set rx queue to full.
  R.set (R.RDH id) 0
  R.set (R.RDT id) $ fromIntegral (numRxQueueEntries - 1)

  return $! queue
  where rxdctlEnable = 0x02000000


initTx
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => Int -- ^ The amount of tx queues that will be initialized.
  -> m [Ptr TransmitDescriptor] -- ^ The initialized tx queues.
initTx numTx = do
  deviceId <- showDeviceId
  $(logInfo) $ deviceId <> " Initializing " <> show numTx <> " tx queues."

  -- Enable CRC offloading and small packet padding.
  R.setMask R.HLREG0 crcPadEnable

  -- Set packet buffer sizes.
  R.set (R.TXPBSIZE 0) bufferSize
  forM_ [1 .. 7] (\i -> R.set (R.TXPBSIZE i) 0)

  -- Required flags, when DCB/VTd are disabled.
  R.set R.DTXMXSZRQ 0xFFFF
  R.clearMask R.RTTDCS arbiterDisable

  -- Do per-queue configuration.
  txQueues <- mapM initQueue [0 .. (numTx - 1)]

  -- Enable DMA.
  -- NB: This MUST be done, before enabling any queue, otherwise the queue just won't enable.
  R.set R.DMATXCTL dmaTxEnable

  return txQueues
 where
  dmaTxEnable    = 0x1
  arbiterDisable = 0x40
  crcPadEnable   = 0x401
  bufferSize     = 0xA000
  initQueue
    :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
    => Int
    -> m (Ptr TransmitDescriptor)
  initQueue id = do
    deviceId <- showDeviceId
    $(logDebug) $ deviceId <> " Initializing tx queue " <> show id <> "."

    -- Setup descriptor ring.
    let size = numTxQueueEntries * sizeOf nullTransmitDescriptor
    descPtr  <- allocateDescriptors size

    physAddr <- translate descPtr
    R.set (R.TDBAL id) $ fromIntegral (physAddr .&. 0xFFFFFFFF)
    R.set (R.TDBAH id) $ fromIntegral (shift physAddr (-32))
    R.set (R.TDLEN id) $ fromIntegral size

    $(logDebug)
      $  deviceId
      <> " Tx ring "
      <> show id
      <> " at "
      <> show descPtr
      <> "(phy="
      <> T.pack (showHex physAddr "")
      <> ")."

    -- Descriptor writeback magic values.
    R.set (R.TXDCTL id) =<< wbMagic <$> R.get (R.TXDCTL id)

    return $! descPtr
   where
    wbMagic =
      (.|. (36 .|. shift 8 8 .|. shift 4 16))
        . (.&. complement (0x3F .|. shift 0x3F 8 .|. shift 0x3F 16))
startTxQueue
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => (Ptr TransmitDescriptor, Int)
  -> m TxQueue
startTxQueue (descPtr, id) = do
  deviceId <- showDeviceId
  $(logDebug) $ deviceId <> " Starting tx queue " <> show id <> "."

  -- Setup buffers for descriptors.
  -- This differs from ixy.
  let size = assert (numTxQueueEntries .&. (numTxQueueEntries - 1) == 0)
                    (numTxQueueEntries * 2048)
  bufPtr <- allocateRaw size True

  -- Tx starts out empty.
  R.set (R.TDH id) 0
  R.set (R.TDT id) 0

  -- Enable queue and wait.
  R.setMask (R.TXDCTL id) txdctlEnable
  R.waitSet (R.TXDCTL id) txdctlEnable

  mkTxQueueM descPtr bufPtr
  where txdctlEnable = 0x2000000

-- | Wait until the device has set up the link and the link speed is available,
-- or until the maximum wait time is exceeded.
--
-- The function waits for 10ms after each try.
waitForLink :: (MonadIO m, MonadReader Device m, MonadLogger m) => Int -> m ()
waitForLink maxTries = do
  $(logDebug) "Waiting for link..."
  waitUntil 0
 where
  waitUntil numTries | numTries == maxTries =
    $(logDebug) "Maximum wait time for link exceeded."
  waitUntil numTries = do
    speed <- linkSpeed
    case speed of
      LinkNotReady -> do
        liftIO $ usleep 10000
        waitUntil (numTries + 1)
      Link100M -> $(logDebug) "Link speed was set to 100MBit/s"
      Link1G   -> $(logDebug) "Link speed was set to 1GBit/s"
      Link10G  -> $(logDebug) "Link speed was set to 10GBit/s"

-- | Returns the link speed of the device as 'LinkSpeed'.
linkSpeed :: (MonadIO m, MonadReader Device m) => m LinkSpeed
linkSpeed = do
  links <- R.get R.LINKS
  if (links .&. linksUp) == 0
    then return LinkNotReady
    else
      return
        $ let l = links .&. links82599
          in  case l of
                _ | l == links100M -> Link100M
                _ | l == links1G   -> Link1G
                _ | l == links10G  -> Link10G
                _                  -> LinkNotReady
 where
  linksUp    = 0x40000000
  links82599 = 0x30000000
  links100M  = 0x10000000
  links1G    = 0x20000000
  links10G   = 0x30000000

receive :: Device -> Driver.QueueId -> Int -> IO [[Word8]]
receive !dev (Driver.QueueId id) !num = case devRxQueues dev V.!? id of
  Just queue -> do
    !rxIndex <- readIORef (rxqIndexRef queue)
    inner queue rxIndex 0
  Nothing -> return []
 where
  inner queue rxIndex i = do
    let !index = (rxIndex + i) `rem` numRxQueueEntries
    if i == num
      then do
        writeTail index
        return []
      else do
        !descriptor <- peek (rxqDescriptor queue index)
        let status = rdStatus descriptor
        if testBit status 0
          then if not $ testBit status 1
            then throwIO $ userError "Multi-segment packets are not supported."
            else do
              let !(bufPtr, bufPhysAddr) = rxqBuffer queue index
              !buffer <- peekArray (fromIntegral (rdLength descriptor)) bufPtr
              resetReceiveDescriptor queue index bufPhysAddr
              (buffer :) <$> inner queue rxIndex (i + 1)
          else do
            writeTail index
            return []
   where
    writeTail newIndex = do
      runReaderT (R.set (R.RDT id) $ fromIntegral newIndex) dev
      writeIORef (rxqIndexRef queue) newIndex

send :: Device -> Driver.QueueId -> [[Word8]] -> IO ()
send !dev (Driver.QueueId id) !buffers = case devTxQueues dev V.!? id of
  Just queue -> do
    clean queue
    inner queue buffers
    newIndex <- readIORef (txqIndexRef queue)
    runReaderT
      (R.set (R.TDT id) $ fromIntegral $ (newIndex - 1) `mod` numRxQueueEntries)
      dev
  Nothing -> return ()
 where
  clean !queue = do
    !curIndex   <- readIORef (txqIndexRef queue)
    !cleanIndex <- readIORef (txqCleanRef queue)
    let !amount = if curIndex - cleanIndex >= 0
          then curIndex - cleanIndex
          else numTxQueueEntries - cleanIndex + curIndex
    when (amount >= txCleanBatch) $ do
      let !cleanTo = (cleanIndex + txCleanBatch - 1) `mod` numTxQueueEntries
      !descriptor <- peek (txqDescriptor queue cleanTo)
      when (tdStatus descriptor .&. 0x1 /= 0) $ do
        writeIORef (txqCleanRef queue) cleanTo
        clean queue
  inner !queue (buf : bufs) = do
    !curIndex   <- readIORef (txqIndexRef queue)
    !cleanIndex <- readIORef (txqCleanRef queue)
    let !nextIndex = (curIndex + 1) `mod` numTxQueueEntries
    unless (nextIndex == cleanIndex)
      $ let (bufPtr, bufPhysAddr) = txqBuffer queue curIndex
        in  do
              pokeArray bufPtr buf
              setTransmitDescriptor queue curIndex bufPhysAddr (length buf)
              writeIORef (txqIndexRef queue) nextIndex
              inner queue bufs
  inner _ [] = return ()

-- $ Helpers

-- | Shows the device's 'BusDeviceFunction' in the form:
--
-- [BDF]
showDeviceId :: (MonadReader Device m) => m Text
showDeviceId = do
  dev <- ask
  return $ "[" <> unBusDeviceFunction (devBdf dev) <> "]"

allocateDescriptors
  :: (MonadThrow m, MonadIO m, MonadLogger m) => Int -> m (Ptr a)
allocateDescriptors size = do
  descPtr <- allocateRaw size True
  liftIO $ fillBytes descPtr 0xFF size
  return descPtr
