module CPU.Pipeline
  ( cpu
  ) where

import Clash.Prelude

import Data.Maybe (fromMaybe, isJust)

import CPU.Machine
import CPU.Instruction
import CPU.Memory

import qualified Periph.IO as IO

import Utils.Files
import Utils.Maybe (whenMaybe)

progBlobs = ($(getBank "../fib2.bin" 0) :>
             $(getBank "../fib2.bin" 1) :>
             $(getBank "../fib2.bin" 2) :>
             $(getBank "../fib2.bin" 3) :> Nil)

pipelineFetch :: HiddenClockResetEnable dom
              => Vec 4 (MemBlob n 8)
              -> Signal dom (Bool)
              -> Signal dom (Maybe PC)
              -> Signal dom (PC, Instruction)
pipelineFetch imblob stall jump = bundle (idPC, idInst)
  where
    en = not <$> stall

    pc = regEn def en $ fromMaybe <$> (liftA2 (+) pc 4) <*> jump
    inst = instructionMemory imblob <$> pc

    -- Flush fetched instruction on jump
    nop = 0x00000013
    inst' = mux (isJust <$> jump) (pure nop) inst

    -- IF/ID regs:
    idPC   = regEn def en pc
    idInst = regEn nop en inst'

pipelineDecode :: HiddenClockResetEnable dom
               => Signal dom (PC, Instruction)                           -- id
               -> Signal dom (Bool)                                      -- flush on jump
               -> Signal dom (InstCtrl)                                  -- mem
               -> Signal dom (Maybe (RegIndex, RegValue))                -- wb
               -> ( Signal dom (PC, InstCtrl, Vec 2 RegValue, Immediate) -- ex
                  , Signal dom (Bool) )                                  -- stall?
pipelineDecode id flush memInst wb = (bundle (exPC, exInst, exRegs, exImm), stall)
  where
    (pc, raw) = unbundle id

    imm  = getImm <$> raw
    inst = decodeInst <$> raw
    regs = registerFile wb (srcRegs <$> inst)

    stall = needStall <$> inst <*> exInst

    -- Insert nop when stalling or flushing
    inst' = mux cond (pure def) inst
      where cond = (||) <$> stall <*> flush

    -- Adjust for forwarding if needed
    inst'' = withBypass <$> inst' <*> exInst <*> memInst

    -- ID/EX regs:
    exPC   = register def pc
    exInst = register def inst''
    exRegs = register def regs
    exImm  = register def imm

pipelineExecute :: HiddenClockResetEnable dom
                => Signal dom (PC, InstCtrl, Vec 2 RegValue, Immediate) -- ex
                -> Signal dom (RegValue)                                -- wb
                -> ( Signal dom (InstCtrl, AluResult, RegValue)         -- mem
                   , Signal dom (Maybe PC) )                            -- jump?
pipelineExecute ex wbData = (bundle (memInst, memAluRes, memRs2), jumpPC)
  where
    (pc, inst, regs, imm) = unbundle ex

    -- Possibly override register contents with forwarded data
    regs' = bypassRegs <$> (regForw <$> inst)
                       <*> regs
                       <*> memAluRes
                       <*> wbData

    -- Select proper inputs for the ALU
    opands = aluSrcMux <$> (aluSrcs <$> inst)
                       <*> regs'
                       <*> imm
                       <*> pc

    aluOp = instAluOp . action <$> inst
    aluRes = runAlu <$> aluOp <*> opands

    -- Decision whether to jump is taken here.
    -- Don't pipeline the result as it is fed directly into PC register
    jumpPC = runJump . action <$> inst <*> aluRes <*> pc <*> regs' <*> imm

    -- Rs2 for memory stores
    rs2 = (!! 1) <$> regs'

    -- EX/MEM regs:
    memInst   = register def inst
    memAluRes = register def aluRes
    memRs2    = register def rs2

pipelineMemory :: HiddenClockResetEnable dom
               => IO.Device dom a
               -> Signal dom (InstCtrl, AluResult, RegValue) -- mem
               -> (Signal dom (InstCtrl, RegValue), Signal dom a) -- wb
pipelineMemory io mem = (bundle (wbInst, wbData), out)
  where
    (inst, aluRes, rs2) = unbundle mem

    act = action <$> inst
    (rdata, out) = io $ instIO <$> act <*> (fromIntegral <$> aluRes) <*> rs2
    rdata' = signExtendIO <$> act <*> rdata

    -- MEM/WB regs:
    wbAluRes = register def aluRes
    wbInst   = register def inst

    -- Load data comes in the next clock cycle
    wbData = writebackMux . action <$> wbInst <*> wbAluRes <*> rdata'

cpu :: HiddenClockResetEnable dom
    => Vec 4 (MemBlob n 8)
    -> IO.Device dom a
    -> Signal dom a
cpu imblob io = out
  where
    -- Instruction fetch phase
    id = pipelineFetch imblob stall jump

    -- Instruction decode phase
    (ex, stall) = pipelineDecode id flush memInst wbRegData

    -- Execute phase
    (mem, jump) = pipelineExecute ex wbData
    (memInst, _, _) = unbundle mem

    flush = isJust <$> jump

    -- Memory and writeback phases
    (wb, out) = pipelineMemory io mem

    (wbInst, wbData) = unbundle wb
    wbReg = dstReg <$> wbInst

    -- Wrap into a Maybe together with target register
    wbRegData = whenMaybe
              . writesBack
              . action <$> wbInst
                       <*> ((,) <$> wbReg <*> wbData)

