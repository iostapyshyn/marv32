
module System
  ( topEntity
  , sim
  ) where

import Clash.Prelude

import CPU.Pipeline
import Periph.IO
import Periph.Memory

import CPU.Machine
import CPU.Instruction

import Data.Char
import qualified Data.List as L

import System.IO

import Text.Printf

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

-- sim :: IO ()
sim =
  --sequence_ $ printWB <$> sample_lazy @System system
  sequence_ $ maybe (return ()) putChar . fst <$> sample_lazy @System system
  where
    system :: HiddenClockResetEnable dom => Signal dom ( Maybe Char
                                                       , (PC, InstCtrl, Vec 2 AluOpand, Maybe (RegIndex, RegValue)) )
    system = cpu progBlobs debugIO

    printWB :: ( Maybe Char
               , (PC, InstCtrl, Vec 2 AluOpand, Maybe (RegIndex, RegValue))) -> IO ()
    printWB (_, (pc, inst, opands, wb)) = putStr . L.concat $
      [ printf "%04x: " pc', show inst, "\n"
      , "   => ", show . (!!0) $ opands, " ", show . (!!1) $ opands, " -> ", show wb, "\n\n"]

      where pc' :: Int
            pc' = fromIntegral pc
