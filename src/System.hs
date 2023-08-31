module System
  ( sim
  , marv32 ) where

import Clash.Prelude
import Clash.Annotations.TH

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

marv32 :: "clk" ::: Clock System
       -> "rst" ::: Reset System
       -> "out" ::: Signal System ()
marv32 clk rst = exposeClockResetEnable @System (fst <$> cpu progBlobs dataMem) clk rst enableGen

makeTopEntity 'marv32

sim :: Int -> IO ()
sim n = withFile "trace.txt" WriteMode (\h -> mapM_ (go h) sampled)
  where
    system :: HiddenClockResetEnable dom => Signal dom (Maybe Char, Trace)
    system = cpu progBlobs debugIO

    sampled = sampleN @System n system

    go handle (a, trace) = maybe (return ()) putChar a >>
                           hPutStr handle (prettyTrace trace)
