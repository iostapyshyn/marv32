{-# LANGUAGE MagicHash #-}

module Periph.Memory
  ( blobMemory
  ) where

import Clash.Prelude

import CPU.Machine

import qualified Periph.IO as IO

import Utils.Maybe
import Data.Maybe

bankAddr :: MAddr   -- ^ Start address
         -> Index 4 -- ^ Index of the memory bank
         -> MAddr   -- ^ Address for the specified memory bank
bankAddr addr i = (addr + (3 - fromIntegral i)) `div` 4

bankIndex :: Unsigned 2      -- ^ Width of the memory access
          -> MAddr           -- ^ Start address
          -> Index 4         -- ^ Index of the memory bank
          -> Maybe (Index 4) -- ^ Index of the byte in the data word starting from LSB
bankIndex width addr i = whenMaybe filter index
  where index  = fromIntegral $ (4 - addr + fromIntegral i) `mod` 4
        filter = shiftR index (fromIntegral width) == 0

byteIndex :: Unsigned 2      -- ^ Width of the memory access
          -> MAddr           -- ^ Start address
          -> Index 4         -- ^ Index of the byte in the data word starting from LSB
          -> Maybe (Index 4) -- ^ Index of the memory bank for this byte
byteIndex width addr i = whenMaybe filter index
  where index  = fromIntegral $ (addr + fromIntegral i) `mod` 4
        filter = shiftR i (fromIntegral width) == 0

blobMemory :: forall n dom
              . (HiddenClockResetEnable dom, KnownNat n)
           => Vec 4 (MemBlob n 8)
           -> IO.Device dom (Maybe IO.Access)
blobMemory blobs access = (unpack <$> go, access)
  where
    width = fromMaybe 0 <$> (fmap . fmap) IO.width access
    addr = fromMaybe 0 <$> (fmap . fmap) ((`mod` (natToNum @n * 4)) . IO.addr) access
    wdata = fromMaybe Nothing <$> (fmap . fmap) IO.wdata access

    -- Data ready to read in the next clock cycle
    width' = register def width
    addr'  = register def addr

    go = getRdata <$> width' <*> addr' <*>
      ramsB (bankAddrs <$> addr) (getWdatas <$> width <*> addr <*> wdata)

    bankIndices width addr = map (bankIndex width addr) (iterateI (+1) 0)
    byteIndices width addr = map (byteIndex width addr) (iterateI (+1) 0)
    bankAddrs         addr = map (bankAddr        addr) (iterateI (+1) 0)

    getWdatas _     _    Nothing      = repeat Nothing
    getWdatas width addr (Just wdata) = gatherMaybe wdataVec (bankIndices width addr)
      where wdataVec = slice d7  d0  wdata :> slice d15 d8  wdata :>
                       slice d23 d16 wdata :> slice d31 d24 wdata :> Nil

    getRdata width addr rdatas = concatBitVector#
                               . map (fromMaybe 0)
                               . reverse
                               $ gatherMaybe rdatas (byteIndices width addr)

    rams :: HiddenClockResetEnable dom
         => Vec 4 (Signal dom MAddr)
         -> Vec 4 (Signal dom (Maybe (BitVector 8)))
         -> Vec 4 (Signal dom (BitVector 8))
    rams addrs wdatas = ram 0 :> ram 1 :> ram 2 :> ram 3 :> Nil
      where wports = zipWith (\addr wdata -> tuplify <$> addr <*> wdata) addrs wdatas
            tuplify addr = fmap (addr,)
            ram i = blockRamBlob (blobs !! i) (addrs !! i) (wports !! i)

    ramsB addr wdata = bundle $ rams (unbundle addr) (unbundle wdata)
