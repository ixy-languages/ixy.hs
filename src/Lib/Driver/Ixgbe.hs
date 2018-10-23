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
import qualified Lib.Driver.Ixgbe.Register     as R
import           Lib.Driver.Ixgbe.Types
import           Lib.Memory
import           Lib.Pci
import           Lib.Prelude             hiding ( to )

import           Control.Exception              ( assert )
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Logger
import qualified Data.ByteString               as B
import           Data.IORef
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as Storable
import           Foreign.Marshal.Array          ( peekArray
                                                , pokeArray
                                                )
import           Foreign.Marshal.Utils          ( fillBytes )
import           Foreign.Ptr                    ( plusPtr )
import           Foreign.Storable               ( sizeOf
                                                , peek
                                                , poke
                                                )
import           Numeric                        ( showHex )
import           System.IO.Error                ( userError )
import           System.Posix.Unistd            ( usleep )

-- | The number of descriptors a rx queue manages.
numRxQueueEntries :: Int
numRxQueueEntries = 512

-- | The number of descriptors a tx queue manages.
numTxQueueEntries :: Int
numTxQueueEntries = 512

-- | The minimum amount of transmit descriptors to clean in one go.
txCleanBatch :: Int
txCleanBatch = 32

-- | Initializes a IXGBE NIC and returns a 'Driver'.
init
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => BusDeviceFunction -- ^ The 'BusDeviceFunction' of the IXGBE NIC.
  -> Int -- ^ The number of rx queues that will be initialized.
  -> Int -- ^ The number of tx queues that will be initialized.
  -> m Driver.Device
init bdf numRx numTx = do
  $(logInfo) $ "Initializing device " <> unBusDeviceFunction bdf <> "."
  ptr      <- mapResource bdf "resource0"
  (_, dev) <- runStateT
    go
    Device
      { _devBase     = ptr
      , _devBdf      = bdf
      , _devRxQueues = V.empty
      , _devTxQueues = V.empty
      }
  return $ Driver.Device
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
    _         <- liftIO $ stats dev
    rxQueues  <- runReaderT (initRx numRx) dev
    txQueues  <- runReaderT (initTx numTx) dev
    rxQueues' <- runReaderT (mapM startRxQueue $ zip rxQueues [0 ..]) dev
    txQueues' <- runReaderT (mapM startTxQueue $ zip txQueues [0 ..]) dev
    put
      $  dev
      &  devRxQueues
      .~ V.fromList rxQueues'
      &  devTxQueues
      .~ V.fromList txQueues'
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
      -- Notice how the first call here is directly overwritten by the second,
      -- but the original implementation does this.
      R.set R.AUTOC =<< lms <$> R.get R.AUTOC
      R.set R.AUTOC =<< magic <$> R.get R.AUTOC
      R.setMask R.AUTOC anRestart
     where
      anRestart = 0x1000
      lms       = (.|. shift 0x3 13) . (.&. complement (shift 0x7 13))
      magic     = (.|. (shift 0x0 7 :: Word32)) . (.&. complement 0x00000180)

  receive :: Device -> Driver.QueueId -> Int -> IO (V.Vector ByteString)
  receive dev (Driver.QueueId id) batchSize =
    case (dev ^. devRxQueues) V.!? id of
      Just queue -> inner queue
      Nothing    -> return V.empty
   where
    inner queue = do
      curIndex <- queue ^. rxqIndex
      let descPtrs =
            Storable.slice curIndex batchSize (queue ^. rxqDescriptors)
          bufPtrs = Storable.slice curIndex batchSize (queue ^. rxqBuffers)
      pkts <- V.zipWithM readPackets (V.convert descPtrs) (V.convert bufPtrs)
      liftIO $ (queue ^. rxqShift) $ V.length pkts
      newIndex <- liftIO $ queue ^. rxqIndex
      runReaderT (R.set (R.RDT id) $ fromIntegral newIndex) dev
      return $ V.mapMaybe identity pkts
    readPackets descPtr bufPtr = do
      descriptor <- liftIO $ peek descPtr
      if isDone $ rdStatusError descriptor
        then if not $ isEndOfPacket $ rdStatusError descriptor
          then throwM $ userError "Multi-segment packets are not supported."
          else do
            let len = fromIntegral $ rdLength descriptor
            bufPhysAddr <- translate bufPtr
            liftIO $ poke
              descPtr
              ReadRx {rdPacketAddr = bufPhysAddr, rdHeaderAddr = 0}
            buffer <- liftIO $ peekArray len bufPtr
            return $ Just (B.pack buffer)
        else return Nothing
     where
      isDone        = flip testBit 0
      isEndOfPacket = flip testBit 1

  send
    :: Device
    -> Driver.QueueId
    -> V.Vector ByteString
    -> IO (Either (V.Vector ByteString) ())
  send dev (Driver.QueueId id) bufs = case (dev ^. devTxQueues) V.!? id of
    Just queue -> inner queue
    Nothing    -> return $ Left bufs
   where
    inner queue = do
      curIndex    <- liftIO $ queue ^. txqIndex
      cleanIndex  <- liftIO $ queue ^. txqCleanIndex
      cleanIndex' <- clean curIndex cleanIndex queue
      let
        len = V.length bufs
        n   = min (numTxQueueEntries - curIndex - cleanIndex') len
        -- Not very nice to convert here, but zipWithM has problematic signature.
        descPtrs =
          V.convert $ Storable.slice curIndex n (queue ^. txqDescriptors)
        bufPtrs = V.convert $ Storable.slice curIndex n (queue ^. txqBuffers)
      V.mapM_ writeDescriptor $ V.zip3 bufs descPtrs bufPtrs
      -- Advance tail pointer.
      liftIO $ (queue ^. txqShift) n
      liftIO $ (queue ^. txqCleanShift) $ cleanIndex' - cleanIndex
      newIndex <- liftIO $ queue ^. txqIndex
      runReaderT (R.set (R.TDT id) $ fromIntegral newIndex) dev
      if n /= len
        then return $ Left (V.drop (len - n) bufs)
        else return $ Right ()
    writeDescriptor (buf, descPtr, bufPtr) = do
      bufPhysAddr <- translate bufPtr
      liftIO $ do
        pokeArray bufPtr $ B.unpack buf
        let len = B.length buf
        poke
          descPtr
          ReadTx
            { tdBufAddr      = bufPhysAddr
            , tdCmdTypeLen   = fromIntegral $ cmdTypeLen len
            , tdOlInfoStatus = fromIntegral $ shift len 14
            }
     where
      cmdTypeLen = (.|.)
        (   endOfPacket
        .|. reportStatus
        .|. frameCheckSequence
        .|. descExt
        .|. advDesc
        )
       where
        endOfPacket        = 0x1000000
        reportStatus       = 0x8000000
        frameCheckSequence = 0x2000000
        descExt            = 0x20000000
        advDesc            = 0x300000
    clean curIndex cleanIndex queue = do
      let cleanAmount = assert (curIndex >= cleanIndex) (curIndex - cleanIndex)
      if cleanAmount < txCleanBatch
        then return cleanIndex
        else do
          let descriptors = queue ^. txqDescriptors
              cleanBound  = cleanIndex + txCleanBatch - 1
          case descriptors Storable.!? cleanBound of
            Just descPtr -> do
              descriptor <- liftIO $ peek descPtr
              return $ if isDone $ tdStatus descriptor
                then cleanBound
                else cleanIndex
            Nothing -> throwM $ userError "Descriptor index was out of bounds."
      where isDone = flip testBit 0

  stats :: Device -> IO Driver.Stats
  stats = runReaderT
    (do
      rxPkts <- R.get R.GPRC
      txPkts <- R.get R.GPTC
      return Driver.Stats
        { Driver.stRxPkts = fromIntegral rxPkts
        , Driver.stTxPkts = fromIntegral txPkts
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
  -> m [RxQueue] -- ^ The initialized queues.
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
    -> m RxQueue
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

    let size = numRxQueueEntries * sizeOf (undefined :: ReceiveDescriptor)
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

    -- Setup RxQueue.
    indexRef <- liftIO $ newIORef (0 :: Int)
    -- We essentially generate two copies of the descriptor vector and append it.
    -- So we can slice from the vector without weird wrapping action and the vector is
    -- immutable anyway.
    let
      descPtrs = Storable.generate
        (2 * numRxQueueEntries)
        (generatePtrs descPtr
                      (sizeOf (undefined :: ReceiveDescriptor))
                      numRxQueueEntries
        )
      fIndex = readIORef indexRef
      fShift n = modifyIORef
        indexRef
        (\current -> (current + n) `mod` numRxQueueEntries)
    return RxQueue
      { _rxqDescriptors = descPtrs
      , _rxqBuffers     = undefined
      , _rxqIndex       = fIndex
      , _rxqShift       = fShift
      }
    where dropEnable = 0x10000000
startRxQueue
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => (RxQueue, Int)
  -> m RxQueue
startRxQueue (queue, id) = do
  deviceId <- showDeviceId
  $(logDebug) $ deviceId <> " Starting rx queue " <> show id <> "."

  -- Setup buffers for descriptors.
  -- Unlike the original implementation we identity map buffers to descriptors
  -- and do not change this anymore.
  let size = assert (numRxQueueEntries .&. (numRxQueueEntries - 1) == 0)
                    (numRxQueueEntries * 2048)
  bufPtr <- allocateRaw size False
  -- Again, look at how the descriptor vector in initQueue is generated to
  -- understand this.
  -- Map the buffers to their descriptors now.
  let bufPtrs = Storable.generate
        (2 * numRxQueueEntries)
        (generatePtrs bufPtr 2048 numRxQueueEntries)
      descPtrs = queue ^. rxqDescriptors
  Storable.zipWithM_ writeDescriptor descPtrs bufPtrs

  -- Enable queue and wait.
  R.setMask (R.RXDCTL id) rxdctlEnable
  R.waitSet (R.RXDCTL id) rxdctlEnable

  -- Set rx queue to full.
  R.set (R.RDH id) 0
  R.set (R.RDT id) $ fromIntegral (numRxQueueEntries - 1)

  return $ queue & rxqBuffers .~ bufPtrs
 where
  rxdctlEnable = 0x02000000
  writeDescriptor descPtr bufPtr = do
    bufPhysAddr <- translate bufPtr
    liftIO $ poke descPtr ReadRx {rdPacketAddr = bufPhysAddr, rdHeaderAddr = 0}

-- | Initialize a number of tx queues.
initTx
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => Int -- ^ The amount of tx queues that will be initialized.
  -> m [TxQueue] -- ^ The initialized tx queues.
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
  -- NB: This MUST be done before enabling any queue, otherwise the queue just won't enable.
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
    -> m TxQueue
  initQueue id = do
    deviceId <- showDeviceId
    $(logDebug) $ deviceId <> " Initializing tx queue " <> show id <> "."

    -- Setup descriptor ring.
    let size = numTxQueueEntries * sizeOf (undefined :: TransmitDescriptor)
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

    -- Setup TxQueue.
    indexRef <- liftIO $ newIORef (0 :: Int)
    cleanRef <- liftIO $ newIORef (0 :: Int)
    let
      descPtrs = Storable.generate
        (2 * numTxQueueEntries)
        (generatePtrs descPtr
                      (sizeOf (undefined :: TransmitDescriptor))
                      numTxQueueEntries
        )
      fIndex = readIORef indexRef
      fClean = readIORef cleanRef
      fShift n = modifyIORef
        indexRef
        (\current -> (current + n) `mod` numTxQueueEntries)
      fCleanShift n = modifyIORef
        indexRef
        (\current -> (current + n) `mod` numTxQueueEntries)
    return TxQueue
      { _txqDescriptors = descPtrs
      , _txqBuffers     = undefined
      , _txqIndex       = fIndex
      , _txqCleanIndex  = fClean
      , _txqShift       = fShift
      , _txqCleanShift  = fCleanShift
      }
   where
    wbMagic =
      (.|. (36 .|. shift 8 8 .|. shift 4 16))
        . (.&. complement (0x3F .|. shift 0x3F 8 .|. shift 0x3F 16))
startTxQueue
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => (TxQueue, Int)
  -> m TxQueue
startTxQueue (queue, id) = do
  deviceId <- showDeviceId
  $(logDebug) $ deviceId <> " Starting tx queue " <> show id <> "."

  -- Setup buffers for descriptors.
  -- This differs from ixy.
  let size = assert (numTxQueueEntries .&. (numTxQueueEntries - 1) == 0)
                    (numTxQueueEntries * 2048)
  bufPtr <- allocateRaw size False
  let bufPtrs = Storable.generate
        (2 * numTxQueueEntries)
        (generatePtrs bufPtr 2048 numTxQueueEntries)

  -- Tx starts out empty.
  R.set (R.TDH id) 0
  R.set (R.TDT id) 0

  -- Enable queue and wait.
  R.setMask (R.TXDCTL id) txdctlEnable
  R.waitSet (R.TXDCTL id) txdctlEnable

  return $ queue & txqBuffers .~ bufPtrs
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

-- $ Helpers

-- | Shows the device's 'BusDeviceFunction' in the form:
--
-- [BDF]
showDeviceId :: (MonadReader Device m) => m Text
showDeviceId = do
  dev <- ask
  return $ "[" <> (dev ^. devBdf . to unBusDeviceFunction) <> "]"

generatePtrs :: Ptr a -> Int -> Int -> Int -> Ptr a
generatePtrs ptr offset num i = ptr `plusPtr` ((i `mod` num) * offset)

allocateDescriptors
  :: (MonadThrow m, MonadIO m, MonadLogger m) => Int -> m (Ptr a)
allocateDescriptors size = do
  descPtr <- allocateRaw size True
  liftIO $ fillBytes descPtr 0xFF size
  return descPtr
