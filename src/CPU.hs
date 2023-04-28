module CPU where

import qualified Data.List as L
import Clash.Prelude

import Data.Maybe (isJust, fromMaybe)
import Data.Default

import CPU.Types
import CPU.RegFile
import CPU.Decode
import CPU.ALU
import CPU.Memory

import Utils.Files
import Utils

progBlobs = ($(getBank "../t.bin" 0) :>
             $(getBank "../t.bin" 1) :>
             $(getBank "../t.bin" 2) :>
             $(getBank "../t.bin" 3) :> Nil)

pipelineFetch :: HiddenClockResetEnable dom
              => Signal dom (Maybe PC)
              -> Signal dom (PC, InstRaw)
pipelineFetch jump = bundle (pc, instMem progBlobs <$> pc)
  where pc = register 0 $ liftA2 fromMaybe (liftA2 (+) pc 4) jump

pipelineDecode :: HiddenClockResetEnable dom
               => Signal dom (PC, InstRaw)
               -> Signal dom (Maybe (RegIndex, MWordS))
               -> Signal dom (PC, InstDecoded, MWordS, MWordS)
pipelineDecode i wb = bundle (pc', instDecoded', rs1', rs2')
  where (pc, instRaw) = unbundle i
        instDecoded   = instDecode <$> instRaw

        (rs1, rs2)    = unbundle . regFile . bundle $ ( wb
                                                      , (getRs1 <$> instDecoded)
                                                      , (getRs2 <$> instDecoded) )
        -- regs:
        pc'           = register def pc
        instDecoded'  = register def instDecoded
        rs1'          = register def rs1
        rs2'          = register def rs2

pipelineExecute :: HiddenClockResetEnable dom
                => Signal dom (PC, InstDecoded, MWordS, MWordS)
                -> Signal dom (PC, InstDecoded, MWordS, MWordS)
pipelineExecute i = bundle (pc', instDecoded', aluRes', rs2')
  where (pc, instDecoded, rs1, rs2) = unbundle i
        aluOp = actionOp <$> (getAct <$> instDecoded)
        imm   = getImm <$> instDecoded

        -- prettier?
        opand1 = let src = getSrc1 <$> instDecoded
                 in aluSrcMux <$> src <*> rs1 <*> rs2 <*> imm <*> pc
        opand2 = let src = getSrc2 <$> instDecoded
                 in aluSrcMux <$> src <*> rs1 <*> rs2 <*> imm <*> pc

        aluRes = alu <$> aluOp <*> opand1 <*> opand2

        -- regs:
        instDecoded' = register def instDecoded
        pc'     = register def pc
        aluRes' = register def aluRes
        rs2'    = register def rs2

pipelineMemory :: HiddenClockResetEnable dom
               => Signal dom (PC, InstDecoded, MWordS, MWordS)
               -> Signal dom (InstDecoded, Maybe MWordS)
pipelineMemory i = bundle (instDecoded', out)
  where (pc, instDecoded, aluRes, src) = unbundle i

        action = getAct <$> instDecoded
        rdata = dataMemSE progBlobs action src (fromIntegral <$> aluRes)

        -- regs:
        aluRes'      = register def aluRes
        instDecoded' = register def instDecoded

        writebackMux inst alu mem = case getAct inst of
          Nop         -> Nothing
          MemStore {} -> Nothing
          MemLoad {}  -> Just mem
          ArithLog _  -> Just alu

        out = liftA3 writebackMux instDecoded' aluRes' rdata

pipeline :: HiddenClockResetEnable dom => Signal dom (InstDecoded, Maybe MWordS)
pipeline = memory
  where fetch = pipelineFetch (pure Nothing)
        decode = pipelineDecode fetch wbData
        execute = pipelineExecute decode
        memory = pipelineMemory execute

        (wbInst, wbData') = unbundle memory
        wbReg = getRd <$> wbInst
        wbData = liftA2 (\x y -> liftA2 (,) (pure x) y) wbReg wbData'

topEntity = exposeClockResetEnable @System $ pipeline

sim = simulateN @System 30 pipeline' [1 :: Int]
  where pipeline' _ = pipeline

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
