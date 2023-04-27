module CPU.Decode
  ( InstRaw
  , InstDecoded (..)
  , instDecode
  , AluSrc (..)
  , MWidth (..)
  , MAddr
  , MDetails
  , aluSrcMux
  ) where

import Clash.Prelude

import CPU.Types
import CPU.RegFile
import CPU.ALU

import Data.Either (isLeft)
import Data.Default


data MWidth = Word | Half | Byte -- Width of access
  deriving (Show)
type MAddr  = MWordU             -- Address line

type MDetails = (Unsigned 2, Bool)

instance NFDataX MWidth where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

data AluSrc = SrcRs1 | SrcRs2 | SrcImm | SrcPC | SrcNil
  deriving (Show)

instance Default AluSrc where
  def = SrcNil

aluSrcMux :: AluSrc
          -> MWordS -- rs1
          -> MWordS -- rs2
          -> MWordS -- imm
          -> PC     -- pc
          -> MWordS
aluSrcMux src rs1 rs2 imm pc = case src of
  SrcRs1 -> rs1
  SrcRs2 -> rs2
  SrcImm -> imm
  SrcPC  -> fromIntegral pc
  SrcNil -> 0

type InstRaw  = BitVector 32
type Opcode   = BitVector 7
type Funct7   = BitVector 7
type Funct3   = BitVector 3

data InstDecoded =
  InstDecoded { getAluOp :: AluOp
              , getRs1   :: RegIndex
              , getRs2   :: RegIndex
              , getRd    :: RegIndex
              , getImm   :: MWordS
              , getSrc1  :: AluSrc
              , getSrc2  :: AluSrc
              , getRegWB :: Bool
              , getMemDetails :: Maybe (Bool, MDetails)
              } deriving (Show)

instance NFDataX InstDecoded where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance Default InstDecoded where
  def = InstDecoded { getAluOp = AluAdd
                    , getRs1  = def
                    , getRs2  = def
                    , getRd   = def
                    , getImm  = def
                    , getSrc1 = def
                    , getSrc2 = def
                    , getRegWB = False
                    , getMemDetails = Nothing}

instOpcode :: InstRaw -> Opcode
instOpcode = unpack . slice d6 d0

instFunct7 :: InstRaw -> Funct7
instFunct7 = unpack . slice d31 d25

instFunct3 :: InstRaw -> Funct3
instFunct3 = unpack . slice d14 d12

instRs1 :: InstRaw -> RegIndex
instRs1 = unpack . slice d19 d15

instRs2 :: InstRaw -> RegIndex
instRs2 = unpack . slice d24 d20

instRd :: InstRaw -> RegIndex
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

instImm :: InstRaw -> MWordS
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

instAluOp :: Opcode -> Funct7 -> Funct3 -> (AluOp, AluSrc, AluSrc, Maybe (Bool, MDetails))
instAluOp opcode funct7 funct3 =
  case opcode of
    0x13 -> (iAluOp, SrcRs1, SrcImm, Nothing)
    0x33 -> (rAluOp, SrcRs1, SrcRs2, Nothing)
    0x37 -> (AluAdd, SrcNil, SrcNil, Nothing)
    0x17 -> (AluAdd, SrcPC,  SrcImm, Nothing)
    0x03 -> (AluAdd, SrcRs1, SrcImm, Just (False, loadDetails))
    0x23 -> (AluAdd, SrcRs1, SrcImm, Just (True, storeDetails))
  where
    loadDetails = case funct3 of
      0x0 -> (1, False)                       -- lb
      0x1 -> (2, False)                       -- lh
      0x2 -> (4, False)                       -- lw
    storeDetails = case funct3 of
      0x0 -> (1, True)                       -- lb
      0x1 -> (2, True)                       -- lh
      0x2 -> (4, True)                       -- lw
      0x4 -> (1, False)                      -- lbu
      0x5 -> (2, False)                      -- lhu
    rAluOp = case funct3 of
      0x0 -> AluAdd                             -- addi
      0x1 -> AluSll                             -- slli
      0x2 -> AluSlt                             -- slti
      0x3 -> AluSltu                            -- sltiu
      0x4 -> AluXor                             -- xori
      0x5 -> case funct7 of
        0x00 -> AluSrl                          -- srli
        0x20 -> AluSra                          -- srai
      0x6 -> AluOr                              -- ori
      0x7 -> AluAnd                             -- andi
    iAluOp = case funct7 of
      0x00 -> case funct3 of
        0x0 -> AluAdd                           -- add
        0x1 -> AluSll                           -- sll
        0x2 -> AluSlt                           -- slt
        0x3 -> AluSltu                          -- sltu
        0x4 -> AluXor                           -- xor
        0x5 -> AluSrl                           -- srl
        0x6 -> AluOr                            -- or
        0x7 -> AluAnd                           -- and
      0x20 -> case funct3 of
        0x0 -> AluSub                           -- sub
        0x5 -> AluSra                           -- sra

instDecode :: InstRaw -> InstDecoded
instDecode raw = InstDecoded { getAluOp = aluOp
                             , getSrc1 = aluSrc1
                             , getSrc2 = aluSrc2
                             , getRs1 = instRs1 raw
                             , getRs2 = instRs2 raw
                             , getRd = instRd raw
                             , getImm = instImm raw
                             , getRegWB = True
                             , getMemDetails = memDetails }
  where funct7 = instFunct7 raw
        funct3 = instFunct3 raw
        opcode = instOpcode raw
        (aluOp, aluSrc1, aluSrc2, memDetails) = instAluOp opcode funct7 funct3
