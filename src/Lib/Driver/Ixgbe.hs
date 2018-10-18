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
  (init)
where

import qualified Lib.Driver                    as Driver
import qualified Lib.Driver.Ixgbe.Register     as R
import           Lib.Driver.Ixgbe.Types
import Lib.Memory
import           Lib.Pci
import           Lib.Prelude             hiding ( to )

import Control.Exception (assert)
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Logger
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Storable as Storable
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (plusPtr)
import Foreign.Storable(sizeOf, peek, poke)
import System.IO.Error (userError)
import System.Posix.Unistd (usleep)

numRxQueueEntries :: Int
numRxQueueEntries = 512

numTxQueueEntries :: Int
numTxQueueEntries = 512

init
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => BusDeviceFunction
  -> Int
  -> Int
  -> m Driver.Device
init bdf numRx numTx = do
  $(logInfo) $ "Initializing device " <> unBusDeviceFunction bdf <> "."
  ptr <- mapResource bdf "resource0"
  (_, dev) <- runStateT inner Device {_devBase= ptr, _devBdf=bdf, _devRxQueues=V.empty, _devTxQueues=V.empty}
  return $ Driver.Device {Driver.receive = receive dev, Driver.setPromisc = setPromisc dev, Driver.stats = stats dev }
 where inner = do dev <- get
                  runReaderT (do reset
                                 R.waitSet R.EEC autoReadDone
                                 R.waitSet R.RDRXCTL dmaInitCycleDone
                                 initLink) dev
                  _ <- liftIO $ stats dev
                  rxQueues <- runReaderT (initRx numRx) dev
                  txQueues <- runReaderT (initTx numTx) dev
                  put $ dev & devRxQueues .~ V.fromList rxQueues & devTxQueues .~ V.fromList txQueues
                  liftIO $ setPromisc dev True
                  runReaderT (waitForLink 1000) dev
        where dmaInitCycleDone = 0x8
              autoReadDone = 0x200
              reset = do R.set R.EIMC disableInterrupt
                         R.set R.CTRL resetMask
                         R.waitClear R.CTRL resetMask
                         R.set R.EIMC disableInterrupt
               where resetMask = 0x4000008
                     disableInterrupt = 0x7FFFFFFF
              initLink = R.setMask R.AUTOC anRestart where anRestart = 0x1000
       receive :: Device -> Driver.QueueId -> Int -> IO (V.Vector ByteString)
       receive dev (Driver.QueueId id) batchSize = case (dev ^. devRxQueues) V.!? id of
                                                     Just queue -> inner dev queue
                                                     Nothing -> return V.empty
        where inner dev queue = do curIndex <- queue ^. rxqIndex
                                   let descPtrs = Storable.slice curIndex batchSize (queue ^. rxqDescriptors)
                                       bufPtrs = Storable.slice curIndex batchSize (queue ^. rxqBuffers)
                                    in do pkts <- V.zipWithM readPackets (V.convert descPtrs) (V.convert bufPtrs)
                                          liftIO $ (queue ^. rxqShift) $ V.length pkts
                                          nextIndex <- queue ^. rxqIndex
                                          runReaderT (R.set (R.RDT id) $ fromIntegral nextIndex) dev
                                          return $ V.mapMaybe identity pkts
              readPackets descPtr bufPtr = do descriptor <- liftIO $ peek descPtr
                                              if isDone $ rdStatusError descriptor then
                                                                                   if not $ isEndOfPacket $ rdStatusError descriptor then throwM $ userError "Multi-segment packets are not supported."
                                                                                                                                     else let len = fromIntegral $ rdLength descriptor
                                                                                                                                           in do bufPhysAddr <- translate bufPtr
                                                                                                                                                 liftIO $ poke descPtr ReadRx {rdPacketAddr=bufPhysAddr, rdHeaderAddr=0}
                                                                                                                                                 buffer <- liftIO $ peekArray len bufPtr
                                                                                                                                                 return $ Just (B.pack buffer)
                                                                                   else return Nothing
               where isDone = flip testBit 0
                     isEndOfPacket = flip testBit 1
       stats :: Device -> IO Driver.Stats
       stats = runReaderT (do rxPkts <- R.get R.GPRC
                              txPkts <- R.get R.GPTC
                              return Driver.Stats {Driver.stRxPkts=fromIntegral rxPkts, Driver.stTxPkts=fromIntegral txPkts}) 

       setPromisc :: Device -> Bool -> IO ()
       setPromisc dev flag = runReaderT (if flag then R.setMask R.FCTRL promiscEnable
                                                 else R.clearMask R.FCTRL promiscEnable) dev
        where promiscEnable = 0x300

initRx
  :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m)
  => Int
  -> m [RxQueue]
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
     in do descPtr <- allocateRaw size True
           liftIO $ fillBytes descPtr 0xFF size             

           -- Setup descriptor ring.
           physAddr <- translate descPtr
           R.set (R.RDBAL id) $ fromIntegral (physAddr .&. 0xFFFFFFFF)
           R.set (R.RDBAH id) $ fromIntegral (shift physAddr (-32))
           R.set (R.RDLEN id) $ fromIntegral size

           $(logDebug) $ deviceId <> "Rx ring " <> show id <> " at " <> show descPtr <> "(phy="<> show physAddr <> ")."

           -- Set ring to empty at the start.
           R.set (R.RDH id) 0
           R.set (R.RDT id) 0

           -- Setup RxQueue.
           indexRef <- liftIO $ newIORef (0 :: Int) 
           -- We essentially generate two copies of the descriptor vector and append it.
           -- So we can slice from the vector without weird wrapping action and the vector is
           -- immutable anyway.
           let descPtrs = Storable.generate (2 * (numRxQueueEntries - 1)) (generatePtrs descPtr (sizeOf (undefined :: ReceiveDescriptor)) (numRxQueueEntries - 1))
               len = numRxQueueEntries `quot` 2
               fIndex = readIORef indexRef
               fShift n = modifyIORef indexRef (+ n `mod` len)
            in startRxQueue id RxQueue {_rxqDescriptors = descPtrs, _rxqBuffers = undefined, _rxqIndex = fIndex, _rxqShift = fShift}
   where dropEnable = 0x10000000
         startRxQueue :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m) => Int -> RxQueue -> m RxQueue 
         startRxQueue id queue = do
           deviceId <- showDeviceId
           $(logDebug) $ deviceId <> " Starting rx queue " <> show id <> "."

           -- Setup buffers for descriptors.
           -- Unlike the original implementation we identity map buffers to descriptors
           -- and do not change this anymore.
           let size = assert (numRxQueueEntries .&. (numRxQueueEntries - 1) == 0) (numRxQueueEntries * 2048)
            in do bufPtr <- allocateRaw size False
                  -- Again, look at how the descriptor vector in initQueue is generated to
                  -- understand this.
                  let bufPtrs = Storable.generate (2 * (numRxQueueEntries - 1)) (generatePtrs bufPtr 2048 (numRxQueueEntries - 1))
                      descPtrs = queue ^. rxqDescriptors
                   -- Map the buffers to their descriptors now.
                   in do Storable.zipWithM_ writeDescriptor descPtrs bufPtrs
                         -- Enable queue and wait.
                         R.setMask (R.RXDCTL id) rxdctlEnable
                         R.waitSet (R.RXDCTL id) rxdctlEnable

                         -- Set rx queue to full.
                         R.set (R.RDH id) 0
                         R.set (R.RDT id) $ fromIntegral (numRxQueueEntries - 1)

                         return $ queue & rxqBuffers .~ bufPtrs
            where rxdctlEnable =0x02000000
                  writeDescriptor descPtr bufPtr = do bufPhysAddr <- translate bufPtr
                                                      liftIO $ poke descPtr ReadRx {rdPacketAddr=bufPhysAddr, rdHeaderAddr=0}

initTx :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m) => Int -> m [TxQueue]
initTx numTx = do
  deviceId <- showDeviceId
  $(logInfo) $ deviceId <> " Initializing " <> show numTx <> " tx queues."

  -- Enable CRC offloading and small packet padding.
  R.setMask R.HLREG0 crcPadEnable

  -- Set packet buffer sizes.
  R.set (R.TXPBSIZE 0) bufferSize
  forM_ [1..7] (\i -> R.set (R.TXPBSIZE i) 0)

  -- Required flags, when DCB/VTd are disabled.
  R.set R.DTXMXSZRQ 0xFFFF
  R.clearMask R.RTTDCS arbiterDisable

  -- Do per-queue configuration.
  txQueues <- mapM initQueue [0..(numTx - 1)]

  -- Enable DMA.
  R.set R.DMATXCTL dmaTxEnable

  return txQueues
 where dmaTxEnable = 0x1
       arbiterDisable = 0x40
       crcPadEnable = 0x401
       bufferSize = 0xA000
       initQueue :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m) => Int -> m TxQueue
       initQueue id = do
         deviceId <- showDeviceId
         $(logDebug) $ deviceId <> "Initializing tx queue " <> show id <> "."

         -- Setup descriptor ring.
         let size = numTxQueueEntries * sizeOf (undefined :: TransmitDescriptor)
          in do descPtr <- allocateRaw size True
                liftIO $ fillBytes descPtr 0xFF size

                physAddr <- translate descPtr
                R.set (R.TDBAL id) $ fromIntegral (physAddr .&. 0xFFFFFFFF)
                R.set (R.TDBAH id) $ fromIntegral (shift physAddr (-32))
                R.set (R.TDLEN id) $ fromIntegral size

                $(logDebug) $ deviceId <> "Tx ring " <> show id <> " at " <> show descPtr <> "(phy="<> show physAddr <> ")."

                -- Descriptor writeback magic values.
                R.set (R.TXDCTL id) =<< wbMagic <$> R.get (R.TXDCTL id)

                -- Setup TxQueue.
                indexRef <- liftIO $ newIORef (0 :: Int)
                cleanRef <- liftIO $ newIORef (0 :: Int)
                let descPtrs = Storable.generate (2 * (numTxQueueEntries - 1)) (generatePtrs descPtr (sizeOf (undefined :: TransmitDescriptor)) (numTxQueueEntries -1)) 
                    len = numTxQueueEntries `quot` 2
                    fIndex = readIORef indexRef
                    fClean = readIORef cleanRef
                    fShift n = modifyIORef indexRef (+ n `mod` len)
                    fCleanShift n = modifyIORef indexRef (+ n `mod` len)
                 in startTxQueue id TxQueue {_txqDescriptors =descPtrs, _txqBuffers =undefined, _txqIndex=fIndex, _txqCleanIndex=fClean, _txqShift=fShift, _txqCleanShift=fCleanShift}

         where wbMagic = (.|. (36 .|. shift 8 8 .|. shift 4 16)) . (.&. complement (0x3F .|. shift 0x3F 8 .|. shift 0x3F 16))
               startTxQueue :: (MonadThrow m, MonadIO m, MonadLogger m, MonadReader Device m) => Int -> TxQueue -> m TxQueue
               startTxQueue id queue = do
                 deviceId <- showDeviceId
                 $(logDebug) $ deviceId <> " Starting tx queue " <> show id <> "."

                 -- Setup buffers for descriptors.
                 -- This differs from ixy.
                 let size = assert (numTxQueueEntries .&. (numTxQueueEntries - 1) == 0) (numTxQueueEntries * 2048)
                  in do bufPtr <- allocateRaw (numTxQueueEntries * 2048) False
                        let bufPtrs = Storable.generate (2 * (numTxQueueEntries - 1)) (generatePtrs bufPtr 2048 (numTxQueueEntries - 1))
                        -- Tx starts out empty.
                         in do R.set (R.TDH id) 0
                               R.set (R.RDT id) 0

                               -- Enable queue and wait.
                               R.setMask ( R.TXDCTL id) txdctlEnable
                               R.waitSet (R.TXDCTL id) txdctlEnable

                               return $ queue & txqBuffers .~ bufPtrs
                 where txdctlEnable = 0x2000000

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

showDeviceId :: (MonadReader Device m) => m Text
showDeviceId = do
  dev <- ask
  return $ "[" <> (dev ^. devBdf . to unBusDeviceFunction) <> "]"

generatePtrs :: Ptr a -> Int -> Int -> Int -> Ptr a
generatePtrs ptr offset num i = ptr `plusPtr` ((i `mod` num) * offset)

-- txCleanBatch :: Int
-- txCleanBatch = 32

--   send queueId buffers = do
--     dev <- get
--     case (dev ^. devTxQueues) V.!? queueId of
--                Just queue -> inner queue
--                Nothing -> return $ Fail "Queue id was out of bounds."
--       where inner queue = let curIndex = queue ^. txqIndex
--                               cleanIndex = queue ^. txqCleanIndex
--                                in do cleanIndex' <- clean curIndex cleanIndex queue
--                                      let n = min (abs (curIndex - cleanIndex')) $ V.length buffers
--                                          ptrs = V.take n $ V.zip (V.convert $ splice curIndex n (queue ^. txqDescriptors)) (V.convert $ splice curIndex n (queue ^. txqBuffers))
--                                           in do V.mapM_ writeDescriptor $ V.zip buffers ptrs
--                                                 dev <- get
--                                                 let queue' = queue & txqIndex %~ (+n) & txqCleanIndex .~ cleanIndex'
--                                                     queues = (dev ^. devTxQueues) V.// [(queueId, queue')]
--                                                      in do put $ dev & devTxQueues .~ queues
--                                                            runReaderT (do current <- R.get (R.RDT queueId)
--                                                                           R.set (R.TDT queueId) $ fromIntegral (fromIntegral (fromIntegral current + n) `mod` numTxQueueEntries)) dev
--                                                            return $ if n < V.length buffers then Partial $ V.drop (V.length buffers - n) buffers
--                                                                                             else Done
--             writeDescriptor (buffer, (descPtr, bufPtr)) = do phys <- translate bufPtr
--                                                              liftIO $ do pokeArray bufPtr $ B.unpack buffer
--                                                                          poke descPtr ReadTx {tdBufAddr=phys, tdCmdTypeLen= fromIntegral $ cmdTypeLen (B.length buffer), tdOlInfoStatus = fromIntegral $ shift (B.length buffer) 14}
--              where cmdTypeLen = (.|.) (0x300000 .|. 0x1000000 .|. 0x2000000 .|. 0x8000000 .|. 0x20000000)
--             clean curIndex cleanIndex queue = let cleanNum = assert (curIndex > cleanIndex) (curIndex - cleanIndex)
--                                                    in if cleanNum < txCleanBatch then return cleanIndex
--                                                                                  else let descriptors = queue ^. txqDescriptors
--                                                                                           cleanBound = cleanIndex + txCleanBatch - 1
--                                                                                            in case descriptors Storable.!? cleanBound of
--                                                                                                 Just descPtr -> do descriptor <- liftIO $ peek descPtr
--                                                                                                                    return $ if isDone $ tdStatus descriptor then cleanBound
--                                                                                                                                                             else cleanIndex
--                                                                                                 Nothing -> throwM $ userError "Descriptor index was out of bounds."
--              where isDone = flip testBit 0
