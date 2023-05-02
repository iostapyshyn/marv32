module CPU.Instruction.Action
  ( InstAction (..)
  , AluSrc (..)
  , instAluOp
  , writesBack
  , aluSrcMux
  , writebackMux
  , decodeAction
  , runJump
  ) where

import Clash.Prelude

import CPU.Machine
import CPU.Instruction.Format

-- | Possible sources for the ALU input
data AluSrc = Src0 | Src4 | SrcReg (Index 2) | SrcImm | SrcPC
  deriving (Show, Generic, NFDataX)

aluSrcMux :: Vec 2 MWordS -- ^ Registers
          -> MWordS       -- ^ Immediate
          -> PC           -- ^ PC
          -> AluSrc       -- ^ Source
          -> MWordS
aluSrcMux regs imm pc src = case src of
  Src0     -> 0
  Src4     -> 4
  SrcReg i -> regs !! i
  SrcImm   -> imm
  SrcPC    -> fromIntegral pc

-- | Instruction semantics
data InstAction = Nop
                | ArithLog AluOp
                | MemLoad { width :: Unsigned 2
                          , sign  :: Bool }
                | MemStore { width :: Unsigned 2 }
                | Jump (Maybe Register)
                | Branch Bool AluOp
  deriving (Show, Generic, NFDataX)

-- | Get corresponding ALU operation from the instruction semantics
instAluOp :: InstAction -> AluOp
instAluOp (ArithLog x) = x
instAluOp (Branch _ x) = x
instAluOp _            = AluAdd

-- | Return whether instruction involves a writeback
writesBack :: InstAction -> Bool
writesBack (ArithLog _) = True
writesBack (MemLoad {}) = True
writesBack (Jump _)     = True
writesBack _            = False

writebackMux :: InstAction -> MWordS -> MWordS -> MWordS
writebackMux MemLoad {} _ mem = mem
writebackMux _          alu _ = alu

-- | Evaluate jump/branch depending on ALU result
runJump :: InstAction   -- ^ Instruction type
        -> MWordS       -- ^ ALU result
        -> PC           -- ^ PC
        -> Vec 2 MWordS -- ^ Registers
        -> MWordS       -- ^ Immediate
        -> Maybe PC     -- ^ New program counte
runJump inst aluRes pc regs imm = case inst of
  Jump Nothing  -> Just $ pc + fromIntegral imm
  Jump (Just i) -> Just . fromIntegral $ (regs !! i) + imm
  Branch cond _ | (aluRes /= 0) == cond
                -> Just $ pc + fromIntegral imm
  _             -> Nothing

-- | Extract semantics and ALU operand sources from instruction
decodeAction :: Instruction -> (InstAction, Vec 2 AluSrc)
decodeAction raw =
  case opcode of
    0x13 -> (ArithLog iAluOp, SrcReg 0 :> SrcImm   :> Nil)
    0x33 -> (ArithLog rAluOp, SrcReg 0 :> SrcReg 1 :> Nil)
    0x37 -> (ArithLog AluAdd, Src0     :> SrcImm   :> Nil) -- lui
    0x17 -> (ArithLog AluAdd, SrcPC    :> SrcImm   :> Nil) -- auipc
    0x03 -> (memLoad,         SrcReg 0 :> SrcImm   :> Nil)
    0x23 -> (memStore,        SrcReg 0 :> SrcImm   :> Nil)
    0x6F -> (Jump $ Nothing,  SrcPC    :> Src4     :> Nil) -- jal
    0x67 -> (Jump $ Just 0,   SrcPC    :> Src4     :> Nil) -- jalr
    0x63 -> (branch,          SrcReg 0 :> SrcReg 1 :> Nil)
    0x0F -> (Nop,             Src0     :> Src0     :> Nil) -- fence
    0x73 -> (Nop,             Src0     :> Src0     :> Nil) -- ecall/ebreak
  where
    branch = case funct3 of
      0x0 -> Branch False AluXor                       -- beq
      0x1 -> Branch True  AluXor                       -- bne
      0x4 -> Branch True  AluSlt                       -- blt
      0x5 -> Branch False AluSlt                       -- bge
      0x6 -> Branch True  AluSltu                      -- bltu
      0x7 -> Branch False AluSltu                      -- bgeu
    memStore =
      MemStore { width = unpack $ slice d1 d0 funct3 } -- sb, sh, sw
    memLoad =
      MemLoad { width = unpack $ slice d1 d0 funct3    -- lb, lh, lw,
              , sign = not $ testBit funct3 2 }        -- lbu, lhu
    iAluOp = case funct3 of
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
    rAluOp = case funct7 of
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
      0x01 -> case funct3 of
        0x0 -> AluMul                                  -- mul
        0x1 -> AluMulh                                 -- mulh
        0x2 -> AluMulhsu                               -- mulhsu
        0x3 -> AluMulhu                                -- mulhu
        0x4 -> AluDiv                                  -- div
        0x5 -> AluDivu                                 -- divu
        0x6 -> AluRem                                  -- rem
        0x7 -> AluRemu                                 -- remu

    funct7 = getFunct7 raw
    funct3 = getFunct3 raw
    opcode = getOpcode raw
