module System
  ( topEntity
  , sim
  ) where

import Clash.Prelude

import CPU.Pipeline
import Periph.IO
import Periph.Memory

import Data.Char

import System.IO

import Utils.Files

progBlobs = ($(getBank "../fib.bin" 0) :>
             $(getBank "../fib.bin" 1) :>
             $(getBank "../fib.bin" 2) :>
             $(getBank "../fib.bin" 3) :> Nil)

dataMem :: HiddenClockResetEnable dom => Device dom (Maybe Access)
dataMem = blobMemory progBlobs

debugIO :: HiddenClockResetEnable dom => Device dom (Maybe Char)
debugIO access = unbundle
               $ go <$> access'
                    <*> (bundle $ dataMem access)
  where
    go access (mem, _) = case access of
      Just Access { addr = 0xDEADBEEF
                  , wdata = Just ch }
        -> (0, Just . chr . fromIntegral $ ch)
      _ -> (mem, Nothing)

    access' = register Nothing access

topEntity = exposeClockResetEnable @System $ cpu progBlobs dataMem

sim :: IO ()
sim = sequence_ $ maybe (return ()) putChar <$> sample_lazy @System system
  where
    system :: HiddenClockResetEnable dom => Signal dom (Maybe Char)
    system = cpu progBlobs debugIO
