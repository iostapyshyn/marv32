{-# LANGUAGE MagicHash #-}

module CPU where

import Clash.Prelude
import Data.Maybe
import Text.Printf

import CPU.Types
import CPU.Instruction

instMem :: (SNat n, FilePath) -> PC -> InstRaw
instMem (size,dpath) pc = (concatBitVector#) roms
  where index = shiftR pc 2
        roms = (asyncRomFile @8 size (files !! 3) index :>
                asyncRomFile @8 size (files !! 2) index :>
                asyncRomFile @8 size (files !! 1) index :>
                asyncRomFile @8 size (files !! 0) index :> Nil)
        files = imap (\i x -> printf "rom/%s/%d.rom" x i) (replicate d4 dpath)

instFetch :: HiddenClockResetEnable dom
          => Signal dom (Maybe PC)
          -> Signal dom (PC, InstRaw)
instFetch jump = bundle (pc, instMem (pow2SNat d14, "t") <$> pc)
  where pc = register 0 $ liftA2 fromMaybe (liftA2 (+) pc 4) jump
