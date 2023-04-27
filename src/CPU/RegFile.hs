module CPU.RegFile
  ( RegIndex
  , regFile ) where

import Clash.Prelude

import CPU.Types

type RegIndex = Unsigned 5   -- Register address
type RegFile  = Vec 32 MWordS

regFile :: HiddenClockResetEnable dom
        => Signal dom ( Maybe (RegIndex, MWordS) -- rd write port
                      , RegIndex                 -- rs1 addr
                      , RegIndex )               -- rs2 addr
        -> Signal dom ( MWordS                   -- rs1 data
                      , MWordS )                 -- rs2 data
regFile = mealy f (repeat 0 :: RegFile)
  where -- Transfer function:
    f s (rd, rs1Addr, rs2Addr) = (s' rd, ( s' rd !! rs1Addr
                                         , s' rd !! rs2Addr ))
      where s' (Just (     0,      _)) = s -- r0 is always zero
            s' (Just (rdAddr, rdData)) = replace rdAddr rdData s
            s' (Nothing)               = s
