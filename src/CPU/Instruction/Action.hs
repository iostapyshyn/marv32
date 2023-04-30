module CPU.Instruction.Action
  ( InstAction (..)
  , getAluOp
  , writesBack
  , AluSrc (..)
  , aluSrcMux
  , actionSrcs
  ) where

import Clash.Prelude

import CPU.Machine
import CPU.Instruction.Format

data AluSrc = SrcZero | SrcRs (Index 2) | SrcImm | SrcPC | SrcMem | SrcWB
  deriving (Show, Generic, NFDataX)

aluSrcMux :: AluSrc
          -> ( Vec 2 MWordS -- ^ Registers
             , MWordS       -- ^ Immediate
             , PC           -- ^ PC
             , MWordS       -- ^ Memory stage
             , MWordS )     -- ^ Writeback stage
          -> MWordS
aluSrcMux src (regs,imm,pc,mem,wb) = case src of
  SrcZero -> 0
  SrcRs i -> regs !! i
  SrcImm  -> imm
  SrcPC   -> fromIntegral pc
  SrcMem  -> mem
  SrcWB   -> wb

data InstAction = Nop
                | ArithLog AluOp
                | MemLoad { width :: Unsigned 2
                          , sign  :: Bool }
                | MemStore { width :: Unsigned 2 }
  deriving (Show, Generic, NFDataX)

getAluOp :: InstAction -> AluOp
getAluOp (ArithLog x) = x
getAluOp _            = AluAdd

writesBack :: InstAction -> Bool
writesBack (ArithLog _) = True
writesBack (MemLoad {}) = True
writesBack _            = False

actionSrcs :: Instruction -> (InstAction, Vec 2 AluSrc)
actionSrcs raw =
  case opcode of
    0x13 -> (ArithLog iAluOp, SrcRs 0 :> SrcImm  :> Nil)
    0x33 -> (ArithLog rAluOp, SrcRs 0 :> SrcRs 1 :> Nil)
    0x37 -> (ArithLog AluAdd, SrcZero :> SrcImm  :> Nil)
    0x17 -> (ArithLog AluAdd, SrcPC   :> SrcImm  :> Nil)
    0x03 -> (memLoad        , SrcRs 0 :> SrcImm  :> Nil)
    0x23 -> (memStore       , SrcRs 0 :> SrcImm  :> Nil)
  where
    memStore =
      MemStore { width = unpack $ slice d1 d0 funct3 } -- sb, sh, sw
    memLoad =
      MemLoad { width = unpack $ slice d1 d0 funct3    -- lb, lh, lw,
              , sign = not $ testBit funct3 2 }        -- lbu, lhu
    rAluOp = case funct3 of
      0x0 -> AluAdd                                    -- addi
      0x1 -> AluSll                                    -- slli
      0x2 -> AluSlt                                    -- slti
      0x3 -> AluSltu                                   -- sltiu
      0x4 -> AluXor                                    -- xori
      0x5 -> case funct7 of
        0x00 -> AluSrl                                 -- srli
        0x20 -> AluSra                                 -- srai
      0x6 -> AluOr                                     -- ori
      0x7 -> AluAnd                                    -- andi
    iAluOp = case funct7 of
      0x00 -> case funct3 of
        0x0 -> AluAdd                                  -- add
        0x1 -> AluSll                                  -- sll
        0x2 -> AluSlt                                  -- slt
        0x3 -> AluSltu                                 -- sltu
        0x4 -> AluXor                                  -- xor
        0x5 -> AluSrl                                  -- srl
        0x6 -> AluOr                                   -- or
        0x7 -> AluAnd                                  -- and
      0x20 -> case funct3 of
        0x0 -> AluSub                                  -- sub
        0x5 -> AluSra                                  -- sra

    funct7 = getFunct7 raw
    funct3 = getFunct3 raw
    opcode = getOpcode raw
