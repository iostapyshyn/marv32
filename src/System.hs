
module System
  ( topEntity
  , sim
  ) where

import Clash.Prelude

import CPU.Pipeline
import Periph.IO
import Periph.Memory

import CPU.Trace

import Data.Char

import System.IO

import Utils.Files

progBlobs = $(getBank "app/fib.bin" 0) :>
            $(getBank "app/fib.bin" 1) :>
            $(getBank "app/fib.bin" 2) :>
            $(getBank "app/fib.bin" 3) :> Nil

dataMem :: HiddenClockResetEnable dom => Device dom ()
dataMem = blobMemory progBlobs

debugIO :: HiddenClockResetEnable dom => Device dom (Maybe Char)
debugIO access = unbundle
               $ go <$> access'
                    <*> bundle (dataMem access)
  where
    go access (mem, _) = case access of
      Just Access { addr = 0xDEADBEEF
                  , wdata = Just ch }
        -> (0, Just . chr . fromIntegral $ ch)
      _ -> (mem, Nothing)

    access' = register Nothing access

topEntity = exposeClockResetEnable @System $ cpu progBlobs dataMem

sim :: Int -> IO ()
sim n = withFile "trace.txt" WriteMode (\h -> mapM_ (go h) sampled)
  where
    system :: HiddenClockResetEnable dom => Signal dom (Maybe Char, Trace)
    system = cpu progBlobs debugIO

    sampled = sampleN @System n system

    go handle (a, trace) = maybe (return ()) putChar a >>
                           hPutStr handle (prettyTrace trace)
