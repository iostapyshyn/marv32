module CPU.ALU
  ( AluOp (..)
  ) where

import Clash.Prelude

import CPU.Types

data AluOp = AluAdd | AluSub | AluSll | AluSlt | AluSltu | AluAnd | AluOr
           | AluSrl | AluSra | AluXor
  deriving (Show)

alu :: AluOp -> MWordS -> MWordS -> MWordS
alu op a b =
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
