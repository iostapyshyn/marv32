module CPU where

import Clash.Prelude
import Data.Maybe
import Text.Printf

import CPU.Types
import CPU.Decode
import CPU.Memory

import Utils.Files

import qualified Data.ByteString as B
import qualified Data.List as L
import Language.Haskell.TH

import Data.Word (Word8)

progBlobs = ($(getBank "../t.bin" 0) :>
             $(getBank "../t.bin" 1) :>
             $(getBank "../t.bin" 2) :>
             $(getBank "../t.bin" 3) :> Nil)

instFetch :: HiddenClockResetEnable dom
          => Signal dom (Maybe PC)
          -> Signal dom (PC, InstRaw)
instFetch jump = bundle (pc, instMem progBlobs <$> pc)
  where pc = register 0 $ liftA2 fromMaybe (liftA2 (+) pc 4) jump

topEntity = exposeClockResetEnable @System $ dataMem progBlobs
