{-# LANGUAGE MagicHash #-}

module CPU.Memory
  ( instMem
  , dataMem
  , dataMemSE
  ) where

import Clash.Prelude

import CPU.Types
import CPU.Decode

import Utils
import Data.Maybe

instMem :: Vec 4 (MemBlob n 8) -> PC -> InstRaw
instMem blobs pc = (concatBitVector# . reverse) roms
  where index = shiftR pc 2
        rom i = asyncRomBlob (blobs !! i) index
        roms  = (rom 0 :> rom 1 :> rom 2 :> rom 3 :> Nil)

bankAddr :: MAddr   -- ^ Start address
         -> Index 4 -- ^ Index of the memory bank
         -> MAddr   -- ^ Address for the specified memory bank
bankAddr addr i = (addr + (3 - fromIntegral i)) `div` 4

bankIndex :: Unsigned 2      -- ^ Width of the memory access
          -> MAddr           -- ^ Start address
          -> Index 4         -- ^ Index of the memory bank
          -> Maybe (Index 4) -- ^ Index of the byte in the data word starting from LSB
bankIndex width addr i = whenMaybe index filter
  where index  = fromIntegral $ (4 - addr + fromIntegral i) `mod` 4
        filter = (shiftR index (fromIntegral width)) == 0

byteIndex :: Unsigned 2      -- ^ Width of the memory access
          -> MAddr           -- ^ Start address
          -> Index 4         -- ^ Index of the byte in the data word starting from LSB
          -> Maybe (Index 4) -- ^ Index of the memory bank for this byte
byteIndex width addr i = whenMaybe index filter
  where index  = fromIntegral $ (addr + fromIntegral i) `mod` 4
        filter = (shiftR i (fromIntegral width)) == 0

dataMem :: HiddenClockResetEnable dom
        => Vec 4 (MemBlob n 8)
        -> Signal dom (Unsigned 2)    -- ^ Access width as an exponent of 2
        -> Signal dom (Maybe MWordS)  -- ^ Word to write
        -> Signal dom MAddr           -- ^ Address
        -> Signal dom (BitVector 32)  -- ^ Read data
dataMem blobs width wdata addr = go
  where
    -- Data ready to read in the next clock cycle
    width' = register def width
    addr'  = register def addr

    go = getRdata <$> width' <*> addr' <*>
      (ramsB (bankAddrs <$> addr) (getWdatas <$> width <*> addr <*> wdata))

    bankIndices width addr = map (bankIndex width addr) (iterateI (+1) 0)
    byteIndices width addr = map (byteIndex width addr) (iterateI (+1) 0)
    bankAddrs         addr = map (bankAddr        addr) (iterateI (+1) 0)

    getWdatas _     _    (Nothing)    = repeat Nothing
    getWdatas width addr (Just wdata) = gatherMaybe wdataVec (bankIndices width addr)
      where wdataVec = (slice d7  d0  wdata :> slice d15 d8  wdata :>
                        slice d23 d16 wdata :> slice d31 d24 wdata :> Nil)

    getRdata width addr rdatas = concatBitVector#
                               . map (fromMaybe 0)
                               . reverse
                               $ gatherMaybe rdatas (byteIndices width addr)

    rams :: HiddenClockResetEnable dom
         => Vec 4 (Signal dom (MAddr))
         -> Vec 4 (Signal dom (Maybe (BitVector 8)))
         -> Vec 4 (Signal dom (BitVector 8))
    rams addrs wdatas = (ram 0 :> ram 1 :> ram 2 :> ram 3 :> Nil)
      where wports = zipWith (\addr wdata -> tuplify <$> addr <*> wdata) addrs wdatas
            tuplify addr = fmap ((,) addr)
            ram i = blockRamBlob (blobs !! i) (addrs !! i) (wports !! i)

    ramsB addr wdata = bundle $ rams (unbundle addr) (unbundle wdata)

signExtend2 :: Unsigned 2   -- ^ Width of the input as an exponent of 2
            -> BitVector 32 -- ^ Input
            -> BitVector 32 -- ^ Output
signExtend2 width v = case width of
  0 -> signExtend $ slice d7  d0 v
  1 -> signExtend $ slice d15 d0 v
  _ -> v

dataMemSE :: HiddenClockResetEnable dom
          => Vec 4 (MemBlob n 8)
          -> Signal dom Bool            -- ^ Whether to perform sign extension
          -> Signal dom (Unsigned 2)    -- ^ Access width as an exponent of 2
          -> Signal dom (Maybe MWordS)  -- ^ Word to write
          -> Signal dom MAddr           -- ^ Address
          -> Signal dom MWordS          -- ^ Read data
dataMemSE blobs sext width wdata addr = unpack <$> (extend <$> sext' <*> width' <*> mem)
  where extend True  width v = signExtend2 width v
        extend False _     v = id v

        sext'  = register False sext
        width' = register def width
        mem = dataMem blobs width wdata addr

-- sim = L.zip [0..] $ L.take 10 $ simulate @System dataMem'' input
--   where input = [ (True, 2, Nothing, 0x78)
--                 , (True, 0, Nothing, 0x78)
--                 , (True, 0, Nothing, 0x79)
--                 , (True, 0, Nothing, 0x7a)
--                 , (True, 0, Nothing, 0x7b)
--                 , (True, 0, Nothing, 0x67)
--                 , (True, 0, Nothing, 0x67)
--                 , (True, 0, Nothing, 0x67)
--                 ]
--         dataMem' (a,b,c,d) = pack <$> dataMemSE progBlobs a b c d
--         dataMem'' x = dataMem' $ unbundle x
