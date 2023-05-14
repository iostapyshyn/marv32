module CPU.Machine
  ( Instruction
  , MWordU
  , MWordS
  , MAddr
  , PC
  , RegValue
  , Immediate
  , AluOpand
  , AluResult
  , MemData
  , RegIndex
  , AluOp (..)
  , runAlu
  ) where

import Clash.Prelude

type Instruction = BitVector 32 -- ^ Raw instruction

type MWordU    = Unsigned 32 -- ^ Machine word (Unsigned)
type MWordS    = Signed 32   -- ^ Machine word (Signed)

type MAddr     = MWordU      -- ^ Address line
type PC        = MAddr       -- ^ Program counter
type RegValue  = MWordS      -- ^ Value from a register
type Immediate = MWordS      -- ^ Instruction immediate
type AluOpand  = MWordS      -- ^ ALU operand
type AluResult = MWordS      -- ^ ALU result
type MemData   = MWordS      -- ^ Data from memory

type RegIndex = Unsigned 5   -- ^ Register address

data AluOp = AluAdd | AluSub | AluSll | AluSlt | AluSltu | AluAnd | AluOr
           | AluSrl | AluSra | AluXor | AluMul | AluMulh | AluMulhsu | AluMulhu
           | AluDiv | AluDivu | AluRem | AluRemu
  deriving (Show, Generic, NFDataX)

mulH :: (Integral a, Integral b, Integral c) => a -> b -> c
mulH a b = fromIntegral $ mulH64 (fromIntegral a) (fromIntegral b)
  where mulH64 :: Unsigned 64 -> Unsigned 64 -> Unsigned 64
        mulH64 a64 b64 = shiftR (a64 * b64) 32

runAlu :: AluOp -> Vec 2 AluOpand -> AluResult
runAlu op (a :> b :> Nil) =
  case op of
    AluAdd  -> as + bs
    AluSub  -> as - bs
    AluMul  -> as * bs
    AluMulh   -> mulH as bs
    AluMulhsu -> mulH as bu
    AluMulhu  -> mulH au bu
    AluDiv  -> as `div` bs
    AluRem  -> as `rem` bs
    AluDivu -> fromIntegral $ au `div` bu
    AluRemu -> fromIntegral $ au `rem` bu
    AluSll  -> shiftL as bi
    AluSrl  -> fromIntegral $
               shiftR au bi
    AluSra  -> shiftR as bi
    AluSlt  -> if as < bs  then 1 else 0
    AluSltu -> if au < bu then 1 else 0
    AluAnd  -> as .&. bs
    AluXor  -> xor as bs
    AluOr   -> as .|. bs
  where as = fromIntegral a :: MWordS -- signed
        au = fromIntegral a :: MWordU -- unsigned
        bs = fromIntegral b :: MWordS -- signed
        bu = fromIntegral b :: MWordU -- unsigned
        bi = fromIntegral b :: Int    -- for shift
runAlu _ _ = error ""
