module System
  (
  ) where

import Clash.Prelude
import CPU.Pipeline
import Periph.IO
import Periph.Memory

import Utils.Files

progBlobs = ($(getBank "../fib2.bin" 0) :>
             $(getBank "../fib2.bin" 1) :>
             $(getBank "../fib2.bin" 2) :>
             $(getBank "../fib2.bin" 3) :> Nil)

dataMem :: HiddenClockResetEnable dom => Device dom
dataMem = blobMemory progBlobs

topEntity = exposeClockResetEnable @System $ cpu progBlobs dataMem

sim = sampleN @System 2000 $ cpu progBlobs dataMem
