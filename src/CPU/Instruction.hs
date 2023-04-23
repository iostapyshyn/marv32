module CPU.Instruction
  ( InstRaw
  , InstDecoded
  ) where

import Clash.Prelude

import CPU.Types
import CPU.RegFile
import CPU.ALU

type InstRaw  = BitVector 32
type Opcode   = BitVector 7
type Funct7   = BitVector 7
type Funct3   = BitVector 3


getOpcode :: InstRaw -> Opcode
getOpcode = unpack . slice d6 d0

getFunct7 :: InstRaw -> Funct7
getFunct7 = unpack . slice d31 d25

getFunct3 :: InstRaw -> Funct3
getFunct3 = unpack . slice d14 d12

getRs1 :: InstRaw -> RegIndex
getRs1 = unpack . slice d19 d15

getRs2 :: InstRaw -> RegIndex
getRs2 = unpack . slice d24 d20

getRd :: InstRaw -> RegIndex
getRd = unpack . slice d11 d7

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

data InstDecoded =
  InstDecoded { aluCtrl :: AluOp
              , rs1     :: RegIndex
              , rs2     :: RegIndex
              , rd      :: RegIndex
              , imm     :: MWordS
              } deriving (Show)

getImm :: InstRaw -> MWordS
getImm raw = unpack . go . instFormat . getOpcode $ raw
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

toAluOp :: Opcode -> Funct7 -> Funct3 -> AluOp
toAluOp opcode funct7 funct3 =
  case opcode of
    0x13 -> case funct3 of
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
    0x33 -> case funct7 of
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
instDecode raw = InstDecoded { aluCtrl = toAluOp opcode funct7 funct3
                             , rs1 = getRs1 raw
                             , rs2 = getRs2 raw
                             , rd = getRd raw
                             , imm = getImm raw }
  where funct7 = getFunct7 raw
        funct3 = getFunct3 raw
        opcode = getOpcode raw
