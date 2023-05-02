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
  , bypassRegs
  ) where

import Clash.Prelude

import CPU.Machine

import CPU.Instruction.Format
import CPU.Instruction.Action

data RegForw = ForwMem | ForwWB
  deriving (Show, Generic, NFDataX)

data InstCtrl = InstCtrl
  { action  :: InstAction            -- ^ Architectural effect
  , aluSrcs :: Vec 2 AluSrc          -- ^ ALU sources
  , srcRegs :: Vec 2 Register        -- ^ Source registers
  , regForw :: Vec 2 (Maybe RegForw) -- ^ Forwarding info
  , dstReg  :: Register              -- ^ Destination register
  } deriving (Show, Generic, NFDataX)

instance Default InstCtrl where
  def = InstCtrl { action  = Nop
                 , aluSrcs = repeat SrcZero
                 , srcRegs = repeat 0
                 , regForw = repeat Nothing
                 , dstReg  = 0 }

decode :: Instruction -> InstCtrl
decode raw =
  InstCtrl { action  = act
           , aluSrcs = srcs
           , srcRegs = getSrcRegs raw
           , regForw = repeat Nothing
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
withBypass this ex mem = this { regForw = imap go . regForw $ this }
  where
    go i _
      | writesReg ex  rs = Just ForwMem
      | writesReg mem rs = Just ForwWB
      | otherwise        = Nothing
      where rs = srcRegs this !! i

bypassRegs :: Vec 2 (Maybe RegForw)   -- ^ Forwarding
           -> Vec 2 MWordS            -- ^ Original register values
           -> MWordS                  -- ^ Value in memory phase
           -> MWordS                  -- ^ Value in writeback phase
           -> Vec 2 MWordS
bypassRegs forw regs mem wb = zipWith sel forw regs
  where
    sel (Nothing) reg    = reg
    sel (Just ForwMem) _ = mem
    sel (Just ForwWB)  _ = wb
