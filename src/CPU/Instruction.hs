module CPU.Instruction
  ( Instruction
  , A.InstAction (..)
  , InstCtrl (..)
  , decodeInst
  , F.getImm
  , A.instAluOp
  , A.writesBack
  , needStall
  , withBypass
  , bypassRegs
  , aluSrcMux
  , A.writebackMux
  , A.runJump
  ) where

import Clash.Prelude

import CPU.Machine

import qualified CPU.Instruction.Format as F
import qualified CPU.Instruction.Action as A

data RegForw = ForwNone | ForwMem | ForwWB
  deriving (Show, Generic, NFDataX)

data InstCtrl = InstCtrl
  { action  :: A.InstAction     -- ^ Architectural effect
  , aluSrcs :: Vec 2 A.AluSrc   -- ^ ALU sources
  , srcRegs :: Vec 2 RegIndex   -- ^ Source registers
  , regForw :: Vec 2 RegForw    -- ^ Forwarding info
  , dstReg  :: RegIndex         -- ^ Destination register
  } deriving (Show, Generic, NFDataX)

instance Default InstCtrl where
  def = InstCtrl { action  = A.Nop
                 , aluSrcs = repeat A.Src0
                 , srcRegs = repeat 0
                 , regForw = repeat ForwNone
                 , dstReg  = 0 }

-- | Decode instruction and return control info
decodeInst :: Instruction -> InstCtrl
decodeInst raw =
  InstCtrl { action  = act
           , aluSrcs = srcs
           , srcRegs = F.getSrcRegs raw
           , regForw = repeat ForwNone
           , dstReg  = F.getDstReg raw }
  where (act, srcs) = A.decodeAction raw

-- | Return whether instruction writes to the specified register
writesReg :: InstCtrl -> RegIndex -> Bool
writesReg _    0  = False -- Writes to r0 are ignored anyway
writesReg inst rd = (A.writesBack . action) inst &&
                    dstReg inst == rd

-- | Return whether stall needed is necessary after load
needStall :: InstCtrl -- ^ Current instruction
          -> InstCtrl -- ^ Preceeding instruction in EX
          -> Bool
needStall this ex = case ex of
  InstCtrl { action = A.MemLoad {} } -> or . map go . aluSrcs $ this
  _                                  -> False
  where
    go (A.SrcReg i) = writesReg ex (srcRegs this !! i)
    go _            = False

-- | Adjust the control lines to forward results from preceeding instructions
withBypass :: InstCtrl -- ^ Current instruction
           -> InstCtrl -- ^ Instruction in EX
           -> InstCtrl -- ^ Instruction in MEM
           -> InstCtrl
withBypass this ex mem = this { regForw = imap go . regForw $ this }
  where
    go i _
      | writesReg ex  rs = ForwMem
      | writesReg mem rs = ForwWB
      | otherwise        = ForwNone
      where rs = srcRegs this !! i

-- | Override register contents with forwarded data
bypassRegs :: Vec 2 RegForw  -- ^ Forwarding
           -> Vec 2 RegValue -- ^ Original register values
           -> AluResult      -- ^ Value in memory phase
           -> RegValue       -- ^ Value in writeback phase
           -> Vec 2 RegValue
bypassRegs forw regs mem wb = zipWith sel forw regs
  where
    sel ForwNone reg = reg
    sel ForwMem  _   = mem
    sel ForwWB   _   = wb

aluSrcMux :: Vec 2 A.AluSrc -- ^ ALU sources
          -> Vec 2 RegValue -- ^ Registers
          -> Immediate      -- ^ Immediate
          -> PC             -- ^ PC
          -> Vec 2 AluOpand
aluSrcMux srcs regs imm pc = map (A.aluSrcMux regs imm pc) srcs
