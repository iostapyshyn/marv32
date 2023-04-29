module CPU.Decode
  ( Instruction
  , InstCtrl (..)
  , instDecode
  , instImm
  , AluSrc (..)
  , InstAction (..)
  , actionOp
  , hasWriteback
  , aluSrcMux
  ) where

import Clash.Prelude

import CPU.Types
import CPU.RegFile
import CPU.ALU

data InstAction = Nop
                | ArithLog AluOp
                | MemLoad { width :: Unsigned 2
                          , sign  :: Bool }
                | MemStore { width :: Unsigned 2 }
  deriving (Show, Generic, NFDataX)

instance Default InstAction where def = Nop

actionOp :: InstAction -> AluOp
actionOp (ArithLog x) = x
actionOp _            = AluAdd

hasWriteback :: InstAction -> Bool
hasWriteback (ArithLog _) = True
hasWriteback MemLoad {}   = True
hasWriteback _            = False

data AluSrc = SrcNil | SrcRs (Index 2) | SrcImm | SrcPC | SrcEx | SrcMem
  deriving (Show, Generic, NFDataX)

instance Default AluSrc where
  def = SrcNil

aluSrcMux :: AluSrc
          -> ( Vec 2 MWordS -- ^ Registers
             , MWordS       -- ^ Immediate
             , PC           -- ^ PC
             , MWordS       -- ^ Execute stage
             , MWordS )     -- ^ Memory stage
          -> MWordS
aluSrcMux src (rs,imm,pc,ex,mem) = case src of
  SrcNil  -> 0
  SrcRs i -> rs !! i
  SrcImm  -> imm
  SrcPC   -> fromIntegral pc
  SrcEx   -> ex
  SrcMem  -> mem

type Instruction = BitVector 32
type Opcode      = BitVector 7
type Funct7      = BitVector 7
type Funct3      = BitVector 3

data InstCtrl = InstCtrl
  { getAct  :: InstAction     -- ^ Architectural effect
  , getSrcs :: Vec 2 AluSrc   -- ^ ALU sources
  , getRs   :: Vec 2 RegIndex -- ^ Source registers
  , getRd   :: RegIndex       -- ^ Destination register
  } deriving (Show, Generic, NFDataX)

instance Default InstCtrl where
  def = InstCtrl { getAct  = def
                 , getSrcs = repeat def
                 , getRs   = repeat def
                 , getRd   = def }

instOpcode :: Instruction -> Opcode
instOpcode = unpack . slice d6 d0

instFunct7 :: Instruction -> Funct7
instFunct7 = unpack . slice d31 d25

instFunct3 :: Instruction -> Funct3
instFunct3 = unpack . slice d14 d12

instRs1 :: Instruction -> RegIndex
instRs1 = unpack . slice d19 d15

instRs2 :: Instruction -> RegIndex
instRs2 = unpack . slice d24 d20

instRd :: Instruction -> RegIndex
instRd = unpack . slice d11 d7

data InstFormat = InstR | InstI | InstS
                | InstB | InstU | InstJ

instFormat :: Opcode -> InstFormat
instFormat 0x13 = InstI
instFormat 0x33 = InstR
instFormat 0x37 = InstU
instFormat 0x17 = InstU
instFormat 0x6F = InstJ
instFormat 0x67 = InstI
instFormat 0x63 = InstB
instFormat 0x23 = InstS
instFormat 0x03 = InstI

instImm :: Instruction -> MWordS
instImm raw = unpack . go . instFormat . instOpcode $ raw
  where
    go :: InstFormat -> BitVector 32
    go InstR = 0
    go InstI = signExtend (slice d31 d20 raw)
    go InstS = signExtend (slice d31 d25 raw ++# slice d11 d7 raw)
    go InstU = signExtend (slice d31 d12 raw ++# (0 :: BitVector 12))
    go InstB = signExtend (slice d31 d31 raw ++# slice d7  d7 raw ++#
                           slice d30 d25 raw ++# slice d11 d8 raw)
    go InstJ = signExtend (slice d31 d31 raw ++# slice d19 d12 raw ++#
                           slice d20 d20 raw ++# slice d30 d21 raw)

instAluOp :: Opcode -> Funct7 -> Funct3 -> (InstAction, Vec 2 AluSrc)
instAluOp opcode funct7 funct3 =
  case opcode of
    0x13 -> (ArithLog iAluOp, SrcRs 0 :> SrcImm  :> Nil)
    0x33 -> (ArithLog rAluOp, SrcRs 0 :> SrcRs 1 :> Nil)
    0x37 -> (ArithLog AluAdd, SrcNil  :> SrcImm  :> Nil)
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

instDecode :: Instruction -> InstCtrl
instDecode raw =
  InstCtrl { getAct  = act
           , getSrcs = srcs
           , getRs   = regs
           , getRd   = instRd raw }
  where funct7 = instFunct7 raw
        funct3 = instFunct3 raw
        opcode = instOpcode raw

        regs        = instRs1 raw :> instRs2 raw :> Nil
        (act, srcs) = instAluOp opcode funct7 funct3
