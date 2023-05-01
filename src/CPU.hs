module CPU where

import Clash.Prelude

import Data.Maybe (fromMaybe, isJust)

import CPU.Machine
import CPU.Instruction
import CPU.Memory

import Utils.Files
import Utils (whenMaybe)

progBlobs = ($(getBank "../fib2.bin" 0) :>
             $(getBank "../fib2.bin" 1) :>
             $(getBank "../fib2.bin" 2) :>
             $(getBank "../fib2.bin" 3) :> Nil)

pipelineFetch :: HiddenClockResetEnable dom
              => Signal dom (Bool)
              -> Signal dom (Maybe PC)
              -> Signal dom (PC, Instruction)
pipelineFetch stall jump = bundle (idPC, idInst)
  where
    en = not <$> stall

    pc = regEn def en $ fromMaybe <$> (liftA2 (+) pc 4) <*> jump
    inst = instructionMemory progBlobs <$> pc

    nop = 0x00000013
    instWithFlush = mux (isJust <$> jump) (pure nop) inst

    -- IF/ID regs:
    idPC   = regEn def en pc
    idInst = regEn nop en instWithFlush

pipelineDecode :: HiddenClockResetEnable dom
               => Signal dom (PC, Instruction)                      -- id
               -> Signal dom (Bool)                                 -- flush on jump
               -> Signal dom (InstCtrl)                             -- mem
               -> Signal dom (Maybe (Register, MWordS))             -- wb
               -> ( Signal dom (PC, InstCtrl, Vec 2 MWordS, MWordS) -- ex
                  , Signal dom (Bool) )                             -- stall?
pipelineDecode id flush memInst wb = (bundle (exPC, exInst, exRegs, exImm), stall)
  where
    (pc, raw) = unbundle id

    imm  = getImm <$> raw
    inst = decode <$> raw
    regs = registerFile wb (srcRegs <$> inst)

    stall = needStall <$> inst <*> exInst
    stallOrFlush = (||) <$> stall <*> flush

    instWithStall  = mux stallOrFlush (pure def) inst
    instWithBypass = withBypass <$> instWithStall <*> exInst <*> memInst

    -- ID/EX regs:
    exPC   = register def pc
    exInst = register def instWithBypass
    exRegs = register def regs
    exImm  = register def imm

pipelineExecute :: HiddenClockResetEnable dom
                => Signal dom (PC, InstCtrl, Vec 2 MWordS, MWordS)   -- ex
                -> Signal dom (MWordS)                               -- wb
                -> ( Signal dom (InstCtrl, MWordS, MWordS)           -- mem
                   , Signal dom (Maybe PC) )                         -- jump?
pipelineExecute ex wbData = (bundle (memInst, memAluRes, memRs2), ifJump)
  where
    (pc, inst, regs, imm) = unbundle ex

    aluOp = getAluOp . action <$> inst

    opands = bundle . map aluSrcMux' . unbundle
           $ aluSrcs <$> inst
      where aluSrcMux' src = aluSrcMux <$> src <*> srcs
            srcs = bundle (regs, imm, pc, memAluRes, wbData)

    aluRes = runAlu <$> aluOp <*> opands
    rs2 = (!! 1) <$> regs -- TODO: Forwarding not working

    -- Don't pipeline as this is forwarded directly into PC register
    ifJump = doJump . action <$> inst <*> aluRes <*> pc <*> regs <*> imm

    -- EX/MEM regs:
    memInst   = register def inst
    memAluRes = register def aluRes
    memRs2    = register def rs2

pipelineMemory :: HiddenClockResetEnable dom
               => Signal dom (InstCtrl, MWordS, MWordS) -- mem
               -> Signal dom (InstCtrl, MWordS)         -- wb
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
         => Signal dom (Bool, MWordS, (PC, InstCtrl, Vec 2 MWordS, MWordS), Maybe PC)
pipeline = bundle (stall, wbData, ex, jump)
  where
    -- Instruction fetch
    id = pipelineFetch stall jump

    -- Instruction decode
    (ex, stall) = pipelineDecode id (isJust <$> jump) memInst wbMaybe

    -- Execute
    (mem, jump) = pipelineExecute ex wbData
    (memInst, _, _) = unbundle mem

    -- Memory and writeback
    wb = pipelineMemory mem

    (wbInst, wbData) = unbundle wb
    wbReg = dstReg <$> wbInst

    wbRegData = liftA2 (,) wbReg wbData
    wbMaybe = liftA2 (whenMaybe . writesBack . action) wbInst wbRegData

topEntity = exposeClockResetEnable @System $ pipeline

sim = sampleN @System 2000 $ pipeline
