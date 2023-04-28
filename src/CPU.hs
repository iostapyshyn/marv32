module CPU where

import Clash.Prelude

import Data.Maybe (fromMaybe)

import CPU.Types
import CPU.RegFile
import CPU.Decode
import CPU.ALU
import CPU.Memory

import Utils.Files

progBlobs = ($(getBank "../t.bin" 0) :>
             $(getBank "../t.bin" 1) :>
             $(getBank "../t.bin" 2) :>
             $(getBank "../t.bin" 3) :> Nil)

pipelineFetch :: HiddenClockResetEnable dom
              => Signal dom (Maybe PC)
              -> Signal dom (PC, Instruction)
pipelineFetch jump = bundle (pc, instMem progBlobs <$> pc)
  where pc = register 0 $ fromMaybe <$> (liftA2 (+) pc 4) <*> jump

pipelineDecode :: HiddenClockResetEnable dom
               => Signal dom (PC, Instruction)               -- IF
               -> Signal dom (InstCtrl)                      -- EX
               -> Signal dom (Maybe (RegIndex, MWordS))      -- MEM (Writeback)
               -> Signal dom (PC, InstCtrl, Vec 2 MWordS, MWordS)
pipelineDecode i ex wb = bundle (pcR, instR, rsR, immR)
  where (pc, raw) = unbundle i

        imm  = instImm <$> raw
        inst = instDecode <$> raw
        rs   = regFile wb $ getRs <$> inst

        -- regs:
        pcR   = register def pc
        instR = register def inst
        rsR   = register def rs
        immR  = register def imm

pipelineExecute :: HiddenClockResetEnable dom
                => Signal dom (PC, InstCtrl, Vec 2 MWordS, MWordS)   -- ID
                -> Signal dom (MWordS)                               -- MEM
                -> Signal dom (PC, InstCtrl, MWordS, MWordS)
pipelineExecute i mem = bundle (pcR, instR, aluR, rs2R)
  where (pc, inst, rs, imm) = unbundle i

        aluOp = actionOp <$> (getAct <$> inst)

        opands = let srcs = bundle (rs, imm, pc, aluR, mem)
                     aluSrcMux' src = aluSrcMux <$> src <*> srcs
                 in bundle . map aluSrcMux' . unbundle $ getSrcs <$> inst

        alu = getAlu <$> aluOp <*> opands
        rs2 = (!! 1) <$> rs

        -- regs:
        instR = register def inst
        pcR   = register def pc
        aluR  = register def alu
        rs2R  = register def rs2

pipelineMemory :: HiddenClockResetEnable dom
               => Signal dom (PC, InstCtrl, MWordS, MWordS) -- EX
               -> Signal dom (InstCtrl, Maybe MWordS)
pipelineMemory i = bundle (instR, out)
  where (_, inst, alu, rs2) = unbundle i

        action = getAct <$> inst
        rdata = dataMemSE progBlobs action rs2 (fromIntegral <$> alu)

        writebackMux inst alu mem = case getAct inst of -- TODO: Wrap into maybe in WB
          Nop         -> Nothing
          MemStore {} -> Nothing
          MemLoad {}  -> Just mem
          ArithLog _  -> Just alu

        -- regs:
        aluR  = register def alu
        instR = register def inst

        out = liftA3 writebackMux instR aluR rdata

pipeline :: HiddenClockResetEnable dom => Signal dom (InstCtrl, Maybe MWordS)
pipeline = memory
  where fetch = pipelineFetch (pure Nothing)
        decode = pipelineDecode fetch exInst memData'
        execute = pipelineExecute decode (liftA2 fromMaybe (pure 0) memData)
        memory = pipelineMemory execute

        (_, exInst, _, _) = unbundle execute

        (memInst, memData) = unbundle memory
        memReg = getRd <$> memInst
        memData' = (liftA2 . liftA2) (,) (pure <$> memReg) memData

topEntity = exposeClockResetEnable @System $ pipeline

sim = sampleN @System 30 pipeline

-- sim = L.zip [0..] $ L.take 10 $ simulate @System dataMem'' input
--   where input = [ (2, Nothing, 0)
--                 , (2, Nothing, 0)
--                 , (0, Nothing, 0)
--                 , (0, Nothing, 0)
--                 , (0, Nothing, 0)
--                 , (0, Nothing, 0)
--                 , (0, Nothing, 0)
--                 , (0, Nothing, 0)
--                 ]
--         dataMem' (a,b,c) = pack <$> dataMem progBlobs a b c
--         dataMem'' x = dataMem' $ unbundle x
