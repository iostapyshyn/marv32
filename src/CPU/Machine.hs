module CPU.Machine
  ( Instruction
  , MWordU
  , MWordS
  , MAddr
  , PC
  , Register
  , AluOp (..)
  , runAlu
  ) where

import Clash.Prelude

type Instruction = BitVector 32 -- Raw instruction

type MWordU   = Unsigned 32 -- Machine word (Unsigned)
type MWordS   = Signed 32   -- Machine word (Signed)

type MAddr    = MWordU      -- Address line
type PC       = MAddr       -- Program counter

type Register = Unsigned 5  -- Register address

data AluOp = AluAdd | AluSub | AluSll | AluSlt | AluSltu | AluAnd | AluOr
           | AluSrl | AluSra | AluXor
  deriving (Show, Generic, NFDataX)

runAlu :: AluOp -> Vec 2 MWordS -> MWordS
runAlu op (a :> b :> Nil) =
  case op of
    AluAdd  -> a + b
    AluSub  -> a - b
    AluSll  -> shiftL a bint
    AluSrl  -> fromIntegral $
               shiftR a' bint
    AluSra  -> shiftR a bint
    AluSlt  -> if a  < b  then 1 else 0
    AluSltu -> if a' < b' then 1 else 0
    AluAnd  -> a .&. b
    AluXor  -> xor a b
    AluOr   -> a .|. b
  where a' = fromIntegral a :: MWordU -- unsigned
        b' = fromIntegral b :: MWordU -- unsigned
        bint = fromIntegral b :: Int  -- generic
