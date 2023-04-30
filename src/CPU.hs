module CPU where

import Clash.Prelude

import Data.Maybe (fromMaybe)

import CPU.Machine
import CPU.Instruction
import CPU.Memory

import Utils.Files
import Utils (whenMaybe)

progBlobs = ($(getBank "../t.bin" 0) :>
             $(getBank "../t.bin" 1) :>
             $(getBank "../t.bin" 2) :>
             $(getBank "../t.bin" 3) :> Nil)

pipelineFetch :: HiddenClockResetEnable dom
              => Signal dom (Bool)
              -> Signal dom (Maybe PC)
              -> Signal dom (PC, Instruction)
pipelineFetch stall jump = bundle (idPC, idInst)
  where
    en = not <$> stall

    pc = regEn def en $ fromMaybe <$> (liftA2 (+) pc 4) <*> jump
    inst = instructionMemory progBlobs <$> pc

    -- IF/ID regs:
    idPC   = regEn def en pc
    idInst = regEn nop en inst
      where nop = 0x00000013

pipelineDecode :: HiddenClockResetEnable dom
               => Signal dom (PC, Instruction)                      -- ID
               -> Signal dom (InstCtrl)                             -- MEM
               -> Signal dom (Maybe (Register, MWordS))             -- WB
               -> ( Signal dom (PC, InstCtrl, Vec 2 MWordS, MWordS) -- EX
                  , Signal dom (Bool) )                             -- Stall?
pipelineDecode id memInst wb = (bundle (exPC, exInst, exRegs, exImm), stall)
  where
    (pc, raw) = unbundle id

    imm  = getImm <$> raw
    inst = decode <$> raw
    regs = registerFile wb (srcRegs <$> inst)

    stall = needStall <$> inst <*> exInst

    instWithStall  = mux stall (pure def) inst
    instWithBypass = withBypass <$> instWithStall <*> exInst <*> memInst

    -- ID/EX regs:
    exPC   = register def pc
    exInst = register def instWithBypass
    exRegs = register def regs
    exImm  = register def imm

pipelineExecute :: HiddenClockResetEnable dom
                => Signal dom (PC, InstCtrl, Vec 2 MWordS, MWordS)   -- EX
                -> Signal dom (MWordS)                               -- WB
                -> Signal dom (InstCtrl, MWordS, MWordS)             -- MEM
pipelineExecute ex wbData = bundle (memInst, memAluRes, memRs2)
  where
    (pc, inst, regs, imm) = unbundle ex

    aluOp = getAluOp . action <$> inst

    opands = bundle . map aluSrcMux' . unbundle
           $ aluSrcs <$> inst
      where aluSrcMux' src = aluSrcMux <$> src <*> srcs
            srcs = bundle (regs, imm, pc, memAluRes, wbData)

    aluRes = runAlu <$> aluOp <*> opands
    rs2 = (!! 1) <$> regs

    -- EX/MEM regs:
    memInst   = register def inst
    memAluRes = register def aluRes
    memRs2    = register def rs2

pipelineMemory :: HiddenClockResetEnable dom
               => Signal dom (InstCtrl, MWordS, MWordS) -- MEM
               -> Signal dom (InstCtrl, MWordS)         -- WB
pipelineMemory mem = bundle (wbInst, wbData)
  where
    (inst, aluRes, rs2) = unbundle mem

    act = action <$> inst
    rdata = dataMemorySE progBlobs act rs2 (fromIntegral <$> aluRes)

    -- MEM/WB regs:
    wbAluRes = register def aluRes
    wbInst   = register def inst

    wbData = liftA3 wbMux wbInst wbAluRes rdata
      where
        wbMux (InstCtrl { action = MemLoad {} }) _   mem = mem
        wbMux _                                  alu _   = alu

pipeline :: HiddenClockResetEnable dom
         => Signal dom (Bool, MWordS, (PC, InstCtrl, Vec 2 MWordS, MWordS))
pipeline = bundle (stall, wbData, ex)
  where
    -- Instruction fetch
    id = pipelineFetch stall (pure Nothing)

    -- Instruction decode
    (ex, stall) = pipelineDecode id memInst wbMaybe

    -- Execute
    mem = pipelineExecute ex wbData
    (memInst, _, _) = unbundle mem

    -- Memory and writeback
    wb = pipelineMemory mem

    (wbInst, wbData) = unbundle wb
    wbReg = dstReg <$> wbInst

    wbRegData = liftA2 (,) wbReg wbData
    wbMaybe = liftA2 (whenMaybe . writesBack . action) wbInst wbRegData

topEntity = exposeClockResetEnable @System $ pipeline

sim = sampleN @System 30 $ pipeline
