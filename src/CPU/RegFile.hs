module CPU.RegFile
  ( RegIndex
  , regFile ) where

import Clash.Prelude

import CPU.Types

type RegIndex = Unsigned 5   -- Register address
type RegFile  = Vec 32 MWordS

regFile :: HiddenClockResetEnable dom
        => Signal dom (Maybe (RegIndex, MWordS)) -- ^ rd write port
        -> Signal dom (Vec 2 RegIndex)           -- ^ rs1, rs2 addrs
        -> Signal dom (Vec 2 MWordS)             -- ^ rs1, rs2 datas
regFile rd rs = mealy f (repeat 0 :: RegFile) (bundle (rd, rs))
  where -- Transfer function:
    f s (rd, rs) = (s' rd, ( s' rd !! (rs !! 0) :>
                             s' rd !! (rs !! 1) :> Nil))
      where s' (Just (     0,      _)) = s -- r0 is always zero
            s' (Just (rdAddr, rdData)) = replace rdAddr rdData s
            s' (Nothing)               = s
