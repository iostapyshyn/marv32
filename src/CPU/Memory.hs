{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}

module CPU.Memory
  ( registerFile
  , instructionMemory
  , dataMemory
  , dataMemorySE
  ) where

import Clash.Prelude

import CPU.Machine
import CPU.Instruction

import Utils
import Data.Maybe

type RegFile = Vec 32 MWordS

registerFile :: HiddenClockResetEnable dom
        => Signal dom (Maybe (RegIndex, RegValue)) -- ^ rd write port
        -> Signal dom (Vec 2 RegIndex)             -- ^ rs1, rs2 addrs
        -> Signal dom (Vec 2 RegValue)             -- ^ rs1, rs2 datas
registerFile rd rs = mealy f (repeat 0 :: RegFile) (bundle (rd, rs))
  where -- Transfer function:
    f s (rd, rs) = (s' rd, ( s' rd !! (rs !! 0) :>
                             s' rd !! (rs !! 1) :> Nil))
      where s' (Just (     0,      _)) = s -- r0 is always zero
            s' (Just (rdAddr, rdData)) = replace rdAddr rdData s
            s' (Nothing)               = s

instructionMemory :: Vec 4 (MemBlob n 8) -> PC -> Instruction
instructionMemory blobs pc = (concatBitVector# . reverse) roms
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
bankIndex width addr i = whenMaybe filter index
  where index  = fromIntegral $ (4 - addr + fromIntegral i) `mod` 4
        filter = (shiftR index (fromIntegral width)) == 0

byteIndex :: Unsigned 2      -- ^ Width of the memory access
          -> MAddr           -- ^ Start address
          -> Index 4         -- ^ Index of the byte in the data word starting from LSB
          -> Maybe (Index 4) -- ^ Index of the memory bank for this byte
byteIndex width addr i = whenMaybe filter index
  where index  = fromIntegral $ (addr + fromIntegral i) `mod` 4
        filter = (shiftR i (fromIntegral width)) == 0

dataMemory :: HiddenClockResetEnable dom
        => Vec 4 (MemBlob n 8)
        -> Signal dom (Unsigned 2)   -- ^ Access width as an exponent of 2
        -> Signal dom (Maybe MWordS) -- ^ Word to write
        -> Signal dom MAddr          -- ^ Address
        -> Signal dom (BitVector 32) -- ^ Read data
dataMemory blobs width wdata addr = go
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

dataMemorySE :: HiddenClockResetEnable dom
          => Vec 4 (MemBlob n 8)
          -> Signal dom InstAction      -- ^ Information about the access
          -> Signal dom MWordS          -- ^ Word to write
          -> Signal dom MAddr           -- ^ Address
          -> Signal dom MemData         -- ^ Read data
dataMemorySE blobs action wdata addr = unpack <$> (extend <$> action' <*> mem)
  where extend MemLoad { width, sign = True } = signExtend2 width
        extend _                              = id

        getWdata MemStore {} wdata = Just wdata
        getWdata _           _     = Nothing

        getWidth m = case m of
          MemLoad  {} -> width m
          MemStore {} -> width m
          _           -> 0

        memWidth = getWidth <$> action
        memWdata = getWdata <$> action <*> wdata

        mem      = dataMemory blobs memWidth memWdata addr

        action'  = register Nop action
