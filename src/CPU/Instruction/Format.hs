module CPU.Instruction.Format
  ( Opcode
  , Funct7
  , Funct3
  , getOpcode
  , getFunct7
  , getFunct3
  , getSrcRegs
  , getDstReg
  , getImm
  ) where

import Clash.Prelude

import CPU.Machine

type Opcode      = BitVector 7
type Funct7      = BitVector 7
type Funct3      = BitVector 3

data InstFormat = InstR | InstI | InstS
                | InstB | InstU | InstJ

getOpcode :: Instruction -> Opcode
getOpcode = unpack . slice d6 d0

getFunct7 :: Instruction -> Funct7
getFunct7 = unpack . slice d31 d25

getFunct3 :: Instruction -> Funct3
getFunct3 = unpack . slice d14 d12

getSrcRegs :: Instruction -> Vec 2 Register
getSrcRegs i = map unpack (slice d19 d15 i :> slice d24 d20 i :> Nil)

getDstReg :: Instruction -> Register
getDstReg = unpack . slice d11 d7

getFormat :: Opcode -> InstFormat
getFormat 0x13 = InstI
getFormat 0x33 = InstR
getFormat 0x37 = InstU
getFormat 0x17 = InstU
getFormat 0x6F = InstJ
getFormat 0x67 = InstI
getFormat 0x63 = InstB
getFormat 0x23 = InstS
getFormat 0x03 = InstI

getImm :: Instruction -> MWordS
getImm raw = unpack . go . getFormat . getOpcode $ raw
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
