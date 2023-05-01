module CPU.Instruction
  ( Instruction
  , InstAction (..)
  , getAluOp
  , writesBack
  , InstCtrl (..)
  , decode
  , getImm
  , needStall
  , withBypass
  , aluSrcMux
  , doJump
  ) where

import Clash.Prelude

import CPU.Machine

import CPU.Instruction.Format
import CPU.Instruction.Action

data InstCtrl = InstCtrl
  { action  :: InstAction     -- ^ Architectural effect
  , aluSrcs :: Vec 2 AluSrc   -- ^ ALU sources
  , srcRegs :: Vec 2 Register -- ^ Source registers
  , dstReg  :: Register       -- ^ Destination register
  } deriving (Show, Generic, NFDataX)

instance Default InstCtrl where
  def = InstCtrl { action  = Nop
                 , aluSrcs = repeat SrcZero
                 , srcRegs = repeat 0
                 , dstReg  = 0 }

decode :: Instruction -> InstCtrl
decode raw =
  InstCtrl { action  = act
           , aluSrcs = srcs
           , srcRegs = getSrcRegs raw
           , dstReg  = getDstReg raw }
  where (act, srcs) = actionSrcs raw

writesReg :: InstCtrl -> Register -> Bool
writesReg _    0  = False -- Writes to r0 are ignored anyway
writesReg inst rd = (writesBack . action) inst &&
                    dstReg inst == rd

-- Stall needed if data from preceeding load is required
needStall :: InstCtrl -- ^ Current instruction
          -> InstCtrl -- ^ Preceeding instruction in EX
          -> Bool
needStall this ex = case ex of
  InstCtrl { action = MemLoad {} } -> or . map go . aluSrcs $ this
  _                                -> False
  where
    go (SrcRs i) = writesReg ex (srcRegs this !! i)
    go _         = False

-- Forward results from a preceeding instruction
withBypass :: InstCtrl -- ^ Current instruction
           -> InstCtrl -- ^ Instruction in EX
           -> InstCtrl -- ^ Instruction in MEM
           -> InstCtrl
withBypass this ex mem = this { aluSrcs = map go . aluSrcs $ this }
  where
    go src@(SrcRs i)
      | writesReg ex  rs = SrcMem
      | writesReg mem rs = SrcWB
      | otherwise        = src
      where rs = srcRegs this !! i
    go src = src
