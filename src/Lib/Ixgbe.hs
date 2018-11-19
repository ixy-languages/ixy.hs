{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Lib.Ixgbe
-- Copyright   :  Alex Egger 2018
-- License     :  BSD3
--
-- Maintainer  :  alex.egger96@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Description
--

module Lib.Ixgbe
  ( Device(..)
  , Stats(..)
  , init
  , receive
  , send
  , stats
  , setPromisc
  , dump
  )
where

import           Lib.Ixgbe.Queue
import           Lib.Memory
import           Lib.Pci
import           Lib.Prelude             hiding ( get
                                                , wait
                                                )

import           Control.Monad.Catch
import           Control.Monad.Logger
import qualified Data.ByteString               as B
import           Data.ByteString.Unsafe         ( unsafeUseAsCStringLen )
import           Data.IORef
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Foreign.Marshal.Utils          ( fillBytes
                                                , copyBytes
                                                )
import           Foreign.Ptr                    ( plusPtr
                                                , castPtr
                                                )
import           Foreign.Storable               ( sizeOf
                                                , peekByteOff
                                                , pokeByteOff
                                                , peek
                                                , poke
                                                )
import           Numeric                        ( showHex )
import           System.IO.Error                ( userError )
import           System.Posix.Unistd            ( usleep )

data Device = Device { devBasePtr :: Ptr Word32
                     , devBdf :: BusDeviceFunction
                     , devRxQueues :: V.Vector RxQueue
                     , devTxQueues :: V.Vector TxQueue }

-- $ Initialization

init
  :: (MonadCatch m, MonadThrow m, MonadIO m, MonadLogger m)
  => BusDeviceFunction
  -> Int
  -> Int
  -> m Device
init bdf numRx numTx = do
  $(logDebug) $ "Inititializing device " <> unBusDeviceFunction bdf <> "."
  basePtr              <- mapResource bdf "resource0"
  (rxQueues, txQueues) <- runReaderT
    go
    Device
      { devBasePtr  = basePtr
      , devBdf      = bdf
      , devRxQueues = V.empty
      , devTxQueues = V.empty
      }
  return $! Device
    { devBasePtr  = basePtr
    , devBdf      = bdf
    , devRxQueues = V.fromList rxQueues
    , devTxQueues = V.fromList txQueues
    }
 where
  go = do
    reset
    initLink
    _        <- stats'
    rxQueues <- initRx numRx
    txQueues <- initTx numTx
    setPromisc' True
    waitForLink 10000000
    return (rxQueues, txQueues)
   where
    stats' = do
      dev <- ask
      liftIO $ stats dev
    setPromisc' flag = do
      dev <- ask
      liftIO $ setPromisc dev flag

initRx
  :: (MonadThrow m, MonadIO m, MonadReader Device m, MonadLogger m)
  => Int
  -> m [RxQueue]
initRx numRx = do
  $(logInfo) $ "Initializing " <> show numRx <> " rx queues."
  dev <- ask
  -- Disable Rx while configuring.
  liftIO $ clearMask dev RXCTRL rxEnable
  -- Set packet buffer sizes.
  liftIO $ do
    set dev (RXPBSIZE 0) bSize
    mapM_ (\i -> set dev (RXPBSIZE i) 0) [1 .. 7]
  -- Enable CRC offloading.
  liftIO $ do
    setMask dev HLREG0  crcStrip
    setMask dev RDRXCTL crcStrip
  -- Enable accepting of breadcast packets.
  liftIO $ setMask dev FCTRL broadcastAcceptMode
  -- Do per-queue configuration.
  ptrs <- mapM setupMem [0 .. numRx - 1]
  mapM_ setupQueue $ zip ptrs [0 ..]
  -- No snoop disable.
  liftIO $ setMask dev CTRL_EXT noSnoopDisable
  -- Magic flags for broken feature.
  liftIO
    $ mapM_ (\i -> clearMask dev (DCA_RXCTRL i) $ shift 1 12) [0 .. numRx - 1]
  -- Enable Rx again.
  liftIO $ setMask dev RXCTRL rxEnable

  mapM makeQueue $ zip ptrs [0 ..]
 where
  rxEnable            = 0x1
  bSize               = 0x20000
  crcStrip            = 0x2
  broadcastAcceptMode = 0x400
  noSnoopDisable      = 0x10000
  setupMem _ = do
    descPtr <-
      allocateDescriptors $ numRxQueueEntries * sizeOf nullReceiveDescriptor
    bufPtr <- allocateMem (numRxQueueEntries * bufferSize) True
    let indices = [0 .. numRxQueueEntries - 1]
        descPtrs =
          [ descPtr `plusPtr` (i * sizeOf nullReceiveDescriptor)
          | i <- indices
          ]
        bufPtrs = [ bufPtr `plusPtr` (i * bufferSize) | i <- indices ]
    return (descPtrs, bufPtrs)
  setupQueue ((descPtrs, _), id) = do
    $(logDebug) $ "Initializing rx queue " <> show id <> "."
    -- Enable advanced receive descriptors.
    dev             <- ask
    advRxDescEnable <- fmap
      (.|. (0x02000000 :: Word32))
      ((.&. (0xF1FFFFFF :: Word32)) <$> liftIO (get dev (SRRCTL id)))
    liftIO $ set dev (SRRCTL id) advRxDescEnable
    -- Enable dropping of packets, when all descriptors are full.
    liftIO $ setMask dev (SRRCTL id) dropEnable
    -- Setup descriptor ring.
    case head descPtrs of
      Just descPtr ->
        let (PhysAddr physAddr) = translate (VirtAddr descPtr)
        in  do
              liftIO $ do
                set dev (RDBAL id) $ fromIntegral $ physAddr .&. 0xFFFFFFFF
                set dev (RDBAH id) $ fromIntegral $ shift physAddr (-32)
                set dev (RDLEN id)
                  $ fromIntegral
                  $ length descPtrs
                  * sizeOf nullReceiveDescriptor
              $(logDebug)
                $  "Rx Ring "
                <> show id
                <> " at "
                <> show descPtr
                <> "(phys="
                <> T.pack (showHex physAddr "")
                <> ")."
               -- Set ring to empty at the start.
              liftIO $ do
                set dev (RDH id) 0
                set dev (RDT id) 0
      Nothing -> throwM $ userError "Descriptor pointer list was empty."
    where dropEnable = 0x10000000
  makeQueue ((descPtrs, bufPtrs), id) = do
    dev <- ask
    -- Construct the queue.
    case (head descPtrs, head bufPtrs) of
      (Just descPtr, Just bufPtr) -> do
        queue <- mkRxQueue descPtr bufPtr (length descPtrs)
        $(logDebug) $ "Starting rx queue " <> show id <> "."
        -- Enable queue and wait.
        liftIO $ do
          setMask dev (RXDCTL id) rxdctlEnable
          waitSet dev (RXDCTL id) rxdctlEnable
        -- Set rx queue to full.
        liftIO $ do
          set dev (RDH id) 0
          set dev (RDT id) $ fromIntegral (numRxQueueEntries - 1)

        return $! queue
        where rxdctlEnable = 0x2000000
      _ -> throwM $ userError "This really cant happen can it."

initTx
  :: (MonadThrow m, MonadIO m, MonadReader Device m, MonadLogger m)
  => Int
  -> m [TxQueue]
initTx numTx = do
  $(logInfo) $ "Initializing " <> show numTx <> " tx queues."
  dev <- ask
  -- Enable CRC offloading and small packet padding.
  liftIO $ setMask dev HLREG0 crcPadEnable
  -- Set packet buffer sizes.
  liftIO $ do
    set dev (TXPBSIZE 0) bSize
    mapM_ (\i -> set dev (TXPBSIZE i) 0) [1 .. 7]
  -- Required flags, when DCB/VTd are disabled.
  liftIO $ do
    set dev DTXMXSZRQ 0xFFFF
    clearMask dev RTTDCS arbiterDisable
  -- Do per-queue configuration.
  ptrs <- mapM setupMem [0 .. numTx - 1]
  mapM_ setupQueue $ zip ptrs [0 ..]
  -- Enable DMA.
  liftIO $ set dev DMATXCTL dmaTxEnable

  mapM makeQueue $ zip ptrs [0 ..]
 where
  crcPadEnable   = 0x401
  bSize          = 0xA000
  arbiterDisable = 0x40
  dmaTxEnable    = 0x1
  setupMem _ = do
    descPtr <-
      allocateDescriptors $ numTxQueueEntries * sizeOf nullTransmitDescriptor
    bufPtr <- allocateMem (numTxQueueEntries * bufferSize) True
    let indices = [0 .. numTxQueueEntries - 1]
        descPtrs =
          [ descPtr `plusPtr` (i * sizeOf nullTransmitDescriptor)
          | i <- indices
          ]
        bufPtrs = [ bufPtr `plusPtr` (i * bufferSize) | i <- indices ]
    return (descPtrs, bufPtrs)
  setupQueue ((descPtrs, _), id) = do
    $(logDebug) $ "Initializing tx queue " <> show id <> "."
    dev <- ask
    -- Setup descriptor ring.
    case head descPtrs of
      Just descPtr ->
        let (PhysAddr physAddr) = translate (VirtAddr descPtr)
        in  do
              liftIO $ do
                set dev (TDBAL id) $ fromIntegral $ physAddr .&. 0xFFFFFFFF
                set dev (TDBAH id) $ fromIntegral $ shift physAddr (-32)
                set dev (TDLEN id)
                  $ fromIntegral
                  $ length descPtrs
                  * sizeOf nullTransmitDescriptor
              $(logDebug)
                $  "Tx Ring "
                <> show id
                <> " at "
                <> show descPtr
                <> "(phys="
                <> T.pack (showHex physAddr "")
                <> ")."
              -- Descriptor writeback magic values.
              liftIO $ set dev (TXDCTL id) =<< wbMagic <$> get dev (TXDCTL id)
      Nothing -> throwM $ userError "Descriptor pointer list was empty."
   where
    wbMagic =
      (.|. (36 .|. shift 8 8 .|. shift 4 16))
        . (.&. complement (0x3F .|. shift 0x3F 8 .|. shift 0x3F 16))
  makeQueue ((descPtrs, bufPtrs), id) = do
    dev <- ask
    -- Construct the queue.
    case (head descPtrs, head bufPtrs) of
      (Just descPtr, Just bufPtr) -> do
        queue <- mkTxQueue descPtr bufPtr
        $(logDebug) $ "Starting tx queue " <> show id <> "."
        -- Tx queue starts out empty.
        liftIO $ do
          set dev (TDH id) 0
          set dev (TDT id) 0
        -- Enable queue and wait.
        liftIO $ do
          setMask dev (TXDCTL id) txdctlEnable
          waitSet dev (TXDCTL id) txdctlEnable

        return $! queue
      _ -> throwM $ userError "This really cant happen, can it."
    where txdctlEnable = 0x2000000

initLink :: (MonadIO m, MonadReader Device m) => m ()
initLink = do
  dev <- ask
  -- TODO: If stuff doesn't work check missing init here.
  liftIO $ setMask dev AUTOC anRestart
  where anRestart = 0x1000

reset :: (MonadIO m, MonadReader Device m) => m ()
reset = do
  dev <- ask
  liftIO $ do
    set dev EIMC disableInterrupt
    set dev CTRL resetMask
    waitClear dev CTRL resetMask
    set dev EIMC disableInterrupt
    waitSet dev EEC     autoReadDone
    waitSet dev RDRXCTL dmaInitCycleDone
 where
  resetMask        = 0x4000008
  disableInterrupt = 0x7FFFFFFF
  autoReadDone     = 0x200
  dmaInitCycleDone = 0x8

-- $ Operations

receive :: Device -> Int -> Int -> IO [ByteString]
receive dev id num =
  let queue = devRxQueues dev V.! id
  in  do
        index <- readIORef (rxqIndexRef queue)
        go queue index 0 []
 where
  go queue !index !i !pkts | i == num = do
    let next = (index + i) `rem` numRxQueueEntries
    shiftTail queue next
    return pkts
  go queue !index !i !pkts = do
    let next        = (index + i) `rem` numRxQueueEntries
        descPtr     = {-# SCC "GetDesc" #-} rxqDesc queue next
        bufPtr      = {-# SCC "GetBuf" #-} rxqBuf queue next
        bufPhysAddr = {-# SCC "GetBufPhys" #-} rxqBufPhys queue next
    descriptor <- {-# SCC "PeekDesc" #-} peek descPtr
    if isDone descriptor
      then if not $ isEndOfPacket descriptor
        then throwIO $ userError "Multi-segment packets are not supported."
        else do
-- This had an SCC annotation, but tiffany breaks it. Thx.
          buffer <- {-# SCC "PackBuf" #-} B.packCStringLen
            (castPtr bufPtr, fromIntegral $ rdLength descriptor)
          {-# SCC "PokeResetDesc" #-} poke
            descPtr
            ReceiveRead
              { rdBufPhysAddr = fromIntegral bufPhysAddr
              , rdHeaderAddr  = 0
              }
          let pkts' = {-# SCC "ConsPkt" #-} buffer : pkts
          go queue index (i + 1) pkts'
      else do
        shiftTail queue next
        return pkts
  shiftTail queue newIndex = do
    set dev (RDT id) $ fromIntegral newIndex
    writeIORef (rxqIndexRef queue) newIndex

txCleanBatch :: Int
txCleanBatch = 32

send :: Device -> Int -> [ByteString] -> IO ()
send dev id packets = do
  let queue = devTxQueues dev V.! id
  clean queue
  go queue packets
  newIndex <- readIORef (txqIndexRef queue)
  set dev (TDT id) $ fromIntegral $ (newIndex - 1) `mod` numTxQueueEntries
 where
  go queue (pkt : pkts) = do
    !curIndex   <- {-# SCC "ReadIndex" #-} readIORef (txqIndexRef queue)
    !cleanIndex <- {-# SCC "ReadClean" #-} readIORef (txqCleanRef queue)
    let !nextIndex = {-# SCC "NextIndex" #-} (curIndex + 1) `rem` numTxQueueEntries
    unless (nextIndex == cleanIndex) $ {-# SCC "Working" #-} do
      let
        descPtr            = txqDesc queue curIndex
        bufPtr             = txqBuf queue curIndex
        bufPhysAddr        = txqBufPhys queue curIndex
        size               = B.length pkt
        endOfPacket        = 0x1000000
        reportStatus       = 0x8000000
        frameCheckSequence = 0x2000000
        descExt            = 0x20000000
        advDesc            = 0x300000
        cmdTypeLen         = {-# SCC cmdTypeLen #-} (.|.)
          (   endOfPacket
          .|. reportStatus
          .|. frameCheckSequence
          .|. descExt
          .|. advDesc
          )

      -- TODO: unsafeAsCStringLen maybe?
      {-# SCC "PokeBuf" #-} unsafeUseAsCStringLen pkt (\(ptr, len) -> copyBytes bufPtr (castPtr ptr) len)
      {-# SCC "PokeDesc" #-} poke descPtr TransmitRead { tdBufPhysAddr  = bufPhysAddr , tdCmdTypeLen   = fromIntegral $ cmdTypeLen size , tdOlInfoStatus = fromIntegral $ shift size 14 }
      {-# SCC "BumpIndex" #-} writeIORef (txqIndexRef queue) nextIndex
      go queue pkts
  go _ [] = return ()
  clean !queue = do
    !curIndex   <- readIORef (txqIndexRef queue)
    !cleanIndex <- readIORef (txqCleanRef queue)
    let !amount = if curIndex - cleanIndex >= 0
          then curIndex - cleanIndex
          else numTxQueueEntries - cleanIndex + curIndex
    when (amount >= txCleanBatch) $ do
      let !cleanTo = (cleanIndex + txCleanBatch - 1) `mod` numTxQueueEntries
          descPtr  = txqDesc queue cleanTo
      !descriptor <- peek descPtr
      when (tdStatus descriptor .&. 0x1 /= 0) $ do
        writeIORef (txqCleanRef queue) cleanTo
        clean queue

setPromisc :: Device -> Bool -> IO ()
setPromisc dev flag = if flag
  then setMask dev FCTRL promiscEnable
  else clearMask dev FCTRL promiscEnable
  where promiscEnable = 0x300

data Stats = Stats { stRxPkts :: Int
                   , stTxPkts :: Int
                   , stRxBytes :: Int
                   , stTxBytes :: Int }

stats :: Device -> IO Stats
stats dev = do
  rxPkts   <- get dev GPRC
  txPkts   <- get dev GPTC
  rxBytesL <- get dev GORCL
  rxBytesH <- (`shift` 32) <$> get dev GORCH
  txBytesL <- get dev GOTCL
  txBytesH <- (`shift` 32) <$> get dev GOTCH
  return Stats
    { stRxPkts  = fromIntegral rxPkts
    , stTxPkts  = fromIntegral txPkts
    , stRxBytes = fromIntegral (rxBytesL + rxBytesH)
    , stTxBytes = fromIntegral (txBytesL + txBytesH)
    }

dump :: Device -> IO Text
dump dev = foldr (<>) "" <$> forM [0, 0x2 .. 0xE000] (showRegister . toEnum)
 where
  showRegister register = if register /= UNDEFINED
    then do
      current <- get dev register
      return $ show register <> ": " <> T.pack (showHex current "") <> "\n"
    else return ""

-- $ Memory

allocateDescriptors
  :: (MonadThrow m, MonadIO m, MonadLogger m) => Int -> m (Ptr a)
allocateDescriptors size = do
  descPtr <- allocateMem size True
  liftIO $ fillBytes descPtr 0xFF size
  return descPtr

-- $ Link Speed

data LinkSpeed = LinkNotReady | Link100M | Link1G | Link10G

waitForLink :: (MonadIO m, MonadReader Device m, MonadLogger m) => Int -> m ()
waitForLink timeout = do
  $(logDebug) "Waiting for link..."
  wait 0
 where
  wait numTries | numTries * 10000 >= timeout =
    $(logDebug) "Maximum wait time exceeded."
  wait numTries = linkSpeed >>= \case
    LinkNotReady -> do
      liftIO $ usleep 10000
      wait (numTries + 1)
    Link100M -> $(logDebug) "Link speed was set to 100MBit/s"
    Link1G   -> $(logDebug) "Link speed was set to 1GBit/s"
    Link10G  -> $(logDebug) "Link speed was set to 10GBit/s"
  linkSpeed = do
    dev   <- ask
    links <- liftIO $ get dev LINKS
    if links .&. linksUp == 0
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

-- $ Registers & Register operations

data Register
    = EIMC
    | CTRL
    | AUTOC
    | RXCTRL
    | RXPBSIZE Int
    | HLREG0
    | RDRXCTL
    | FCTRL
    | SRRCTL Int
    | RDBAL Int
    | RDBAH Int
    | RDLEN Int
    | RDH Int
    | RDT Int
    | CTRL_EXT
    | DCA_RXCTRL Int
    | TXPBSIZE Int
    | DTXMXSZRQ
    | RTTDCS
    | GPRC
    | GPTC
    | GORCL
    | GORCH
    | GOTCL
    | GOTCH
    | TDBAL Int
    | TDBAH Int
    | TDLEN Int
    | TXDCTL Int
    | DMATXCTL
    | EEC
    | TDH Int
    | TDT Int
    | LINKS
    | RXDCTL Int
    | CRCERRS
    | TXDGPC
    | UNDEFINED
    deriving (Eq, Show)

instance Enum Register where
    fromEnum EIMC = 0x00888
    fromEnum CTRL = 0x00000
    fromEnum CTRL_EXT = 0x00018
    fromEnum (SRRCTL i)
        | i < 64 = 0x01014 + (i * 0x40)
    fromEnum (SRRCTL i) = 0x0D014 + ((i - 64) * 0x40)
    fromEnum (RDBAL i)
        | i < 64 = 0x01000 + (i * 0x40)
    fromEnum (RDBAL i) = 0x0D000 + ((i - 64) * 0x40)
    fromEnum (RDBAH i)
        | i < 64 = 0x01004 + (i * 0x40)
    fromEnum (RDBAH i) = 0x0D004 + ((i - 64) * 0x40)
    fromEnum (RDLEN i)
        | i < 64 = 0x1008 + (i * 0x40)
    fromEnum (RDLEN i) = 0x0D008 + ((i - 64) * 0x40)
    fromEnum (RDH i)
        | i < 64 = 0x1010 + (i * 0x40)
    fromEnum (RDH i) = 0x0D010 + ((i - 64) * 0x40)
    fromEnum (RDT i)
        | i < 64 = 0x1018 + (i * 0x40)
    fromEnum (RDT i) = 0x0D018 + ((i - 64) * 0x40)
    fromEnum (DCA_RXCTRL i)
        | i < 64 = 0x0100C + (i * 0x40)
    fromEnum (DCA_RXCTRL i) = 0x0D00C + ((i - 64) * 0x40)
    fromEnum (RXDCTL i)
        | i < 64 = 0x01028 + (i * 0x40)
    fromEnum (RXDCTL i) = 0x0D028 + ((i - 64) * 0x40)
    fromEnum RDRXCTL = 0x02F00
    fromEnum RXCTRL = 0x03000
    fromEnum (RXPBSIZE i) = 0x03C00 + (i * 4)
    fromEnum GPRC = 0x04074
    fromEnum GPTC = 0x04080
    fromEnum GORCL = 0x04088
    fromEnum GORCH = 0x0408C
    fromEnum GOTCL = 0x04090
    fromEnum GOTCH = 0x04094
    fromEnum HLREG0 = 0x04240
    fromEnum AUTOC = 0x042A0
    fromEnum LINKS = 0x042A4
    fromEnum RTTDCS = 0x04900
    fromEnum DMATXCTL = 0x04A80
    fromEnum FCTRL = 0x05080
    fromEnum (TDBAL i) = 0x06000 + (i * 0x40)
    fromEnum (TDBAH i) = 0x06004 + (i * 0x40)
    fromEnum (TDLEN i) = 0x06008 + (i * 0x40)
    fromEnum (TDH i) = 0x06010 + (i * 0x40)
    fromEnum (TDT i) = 0x06018 + (i * 0x40)
    fromEnum (TXDCTL i) = 0x06028 + (i * 0x40)
    fromEnum DTXMXSZRQ = 0x08100
    fromEnum (TXPBSIZE i) = 0x0CC00 + (i * 4)
    fromEnum EEC = 0x10010
    fromEnum CRCERRS = 0x4000
    fromEnum UNDEFINED = 0xFFFFF
    fromEnum TXDGPC = 0x87A0
    toEnum 0x00888 = EIMC
    toEnum 0x00000 = CTRL
    toEnum 0x042A0 = AUTOC
    toEnum 0x02F00 = RDRXCTL
    toEnum v
      | v >= 0x03C00 && v <= 0x03C1C && v `mod` 0x4 == 0 = RXPBSIZE ((v - 0x03C00) `quot` 4)
    toEnum v
      | v >= 0x0CC00 && v <= 0xCC1C && v `mod` 0x4 == 0 = TXPBSIZE ((v - 0x0CC00) `quot` 4)
    toEnum 0x04240 = HLREG0
    toEnum 0x03000 = RXCTRL
    toEnum 0x05080 = FCTRL
    toEnum v
        | v >= 0x01000 && v <= 0x01FC0 && v `mod` 0x40 == 0 = RDBAL ((v - 0x01000) `quot` 0x40)
    toEnum v
        | v >= 0x0D000 && v <= 0x0DFC0 && v `mod` 0x40 == 0 = RDBAL (((v - 0x0D000) `quot` 0x40) + 64)
    toEnum v
        | v >= 0x01004 && v <= 0x01FC4 && v `mod` 0x40 == 4 = RDBAH ((v - 0x01004) `quot` 0x40)
    toEnum v
        | v >= 0x0D004 && v <= 0x0DFC4 && v `mod` 0x40 == 4 = RDBAH (((v - 0x0D004) `quot` 0x40) + 64)
    toEnum v
        | v >= 0x01008 && v <= 0x01FC8 && v `mod` 0x40 == 8 = RDLEN ((v - 0x01008) `quot` 0x40)
    toEnum v
        | v >= 0x0D008 && v <= 0x0DFC8 && v `mod` 0x40 == 8 = RDLEN (((v - 0x0D008) `quot` 0x40) + 64)
    toEnum v
        | v >= 0x01010 && v <= 0x01FD0 && v `mod` 0x40 == 0x10 = RDH ((v - 0x01010) `quot` 0x40)
    toEnum v
        | v >= 0x0D010 && v <= 0x0DFD0 && v `mod` 0x40 == 0x10 = RDH (((v - 0x0D010) `quot` 0x40) + 64)
    toEnum v
        | v >= 0x01014 && v <= 0x01FD4 && v `mod` 0x40 == 0x14 = SRRCTL ((v - 0x01014) `quot` 0x40)
    toEnum v
        | v >= 0x0D014 && v <= 0x0D0F4 && v `mod` 0x40 == 0x14 = SRRCTL (((v - 0x0D014) `quot` 0x40) + 64)
    toEnum v
        | v >= 0x01018 && v <= 0x01FD8 && v `mod` 0x40 == 0x18 = RDT ((v - 0x01018) `quot` 0x40)
    toEnum v
        | v >= 0x0D018 && v <= 0x0DFD8 && v `mod` 0x40 == 0x18 = RDT (((v - 0x0D018) `quot` 0x40) + 64)
    toEnum 0x00018 = CTRL_EXT
    toEnum v
        | v >= 0x0100C && v <= 0x01FCC && v `mod` 0x40 == 0xC = DCA_RXCTRL ((v - 0x0100C) `quot` 0x40)
    toEnum v
        | v >= 0x0D00C && v <= 0x0DFCC && v `mod` 0x40 == 0xC = DCA_RXCTRL (((v - 0x0D00C) `quot` 0x40) + 64)
    toEnum v
        | v >= 0x01028 && v <= 0x01FE8 && v `mod` 0x40 == 0x28 = RXDCTL ((v - 0x01028) `quot` 0x40)
    toEnum v
        | v >= 0x0D028 && v <= 0x0DFE8 && v `mod` 0x40 == 0x28 = RXDCTL (((v - 0x0D028) `quot` 0x40) + 64)
    toEnum 0x08100 = DTXMXSZRQ
    toEnum 0x04900 = RTTDCS
    toEnum 0x04074 = GPRC
    toEnum 0x04080 = GPTC
    toEnum 0x04088 = GORCL
    toEnum 0x0408C = GORCH
    toEnum 0x04A80 = DMATXCTL
    toEnum 0x10010 = EEC
    toEnum 0x042A4 = LINKS
    toEnum v
        | v >= 0x06000 && v <= 0x07FC0 && v `mod` 0x40 == 0 = TDBAL ((v - 0x06000) `quot` 0x40)
    toEnum v
        | v >= 0x06004 && v <= 0x07FC4 && v `mod` 0x40 == 4 = TDBAH ((v - 0x06004) `quot` 0x40)
    toEnum v
        | v >= 0x06008 && v <= 0x07FC8 && v `mod` 0x40 == 8 = TDLEN ((v - 0x06008) `quot` 0x40)
    toEnum v
        | v >= 0x06010 && v <= 0x07FD0 && v `mod` 0x40 == 0x10 = TDH ((v - 0x06010) `quot` 0x40)
    toEnum v
        | v >= 0x06018 && v <= 0x07FD8 && v `mod` 0x40 == 0x18 = TDT ((v - 0x06018) `quot` 0x40)
    toEnum v
        | v >= 0x06028 && v <= 0x07FE8 && v `mod` 0x40 == 0x28 = TXDCTL ((v - 0x06028) `quot` 0x40)
    toEnum 0x4000 = CRCERRS
    toEnum 0x87A0 = TXDGPC
    toEnum _ = UNDEFINED

set :: Device -> Register -> Word32 -> IO ()
set dev register = pokeByteOff (devBasePtr dev) (fromEnum register)

get :: Device -> Register -> IO Word32
get dev register = peekByteOff (devBasePtr dev) $ fromEnum register

setMask :: Device -> Register -> Word32 -> IO ()
setMask dev register value = do
  current <- get dev register
  set dev register (current .|. value)

clearMask :: Device -> Register -> Word32 -> IO ()
clearMask dev register value = do
  current <- get dev register
  set dev register (current .&. complement value)

waitUntil :: Device -> Register -> Word32 -> (Word32 -> Bool) -> IO ()
waitUntil dev register value f = do
  current <- get dev register
  if f $ current .&. value
    then return ()
    else do
      liftIO $ usleep 10000
      waitUntil dev register value f

waitSet :: Device -> Register -> Word32 -> IO ()
waitSet dev register value = waitUntil dev register value (== value)

waitClear :: Device -> Register -> Word32 -> IO ()
waitClear dev register value = waitUntil dev register value (== 0)

