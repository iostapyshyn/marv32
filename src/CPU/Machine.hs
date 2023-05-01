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
           | AluSrl | AluSra | AluXor | AluMul | AluMulh | AluMulhsu | AluMulhu
           | AluDiv | AluDivu | AluRem | AluRemu
  deriving (Show, Generic, NFDataX)

mulH :: (Integral a, Integral b, Integral c) => a -> b -> c
mulH a b = fromIntegral $ mulH64 (fromIntegral a) (fromIntegral b)
  where mulH64 :: Unsigned 64 -> Unsigned 64 -> Unsigned 64
        mulH64 a64 b64 = shiftR (a64 * b64) 32

runAlu :: AluOp -> Vec 2 MWordS -> MWordS
runAlu op (a :> b :> Nil) =
  case op of
    AluAdd  -> a + b
    AluSub  -> a - b
    AluMul  -> a * b
    AluMulh  -> mulH a b
    AluMulhsu -> mulH a b'
    AluMulhu -> mulH a' b'
    AluDiv  -> a `div` b
    AluRem  -> a `rem` b
    AluDivu -> fromIntegral $ a' `div` b'
    AluRemu -> fromIntegral $ a' `rem` b'
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
