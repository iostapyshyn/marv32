{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}

module CPU.Memory
  ( registerFile
  , instructionMemory
  ) where

import Clash.Prelude

import CPU.Machine

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
