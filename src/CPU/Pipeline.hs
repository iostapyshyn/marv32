module CPU.Pipeline
  ( cpu
  ) where

import Clash.Prelude

import Data.Maybe (fromMaybe, isJust)

import CPU.Machine
import CPU.Instruction
import CPU.Memory
import CPU.Trace

import qualified Periph.IO as IO

import Utils.Maybe (whenMaybe)

pipelineFetch :: HiddenClockResetEnable dom
              => Vec 4 (MemBlob n 8)
              -> Signal dom Bool
              -> Signal dom (Maybe PC)
              -> Signal dom (PC, Instruction)
pipelineFetch imblob stall jump = bundle (idPC, idInst)
  where
    en = not <$> stall

    pc = regEn def en $ fromMaybe <$> liftA2 (+) pc 4 <*> jump
    inst = instructionMemory imblob <$> pc

    -- Flush fetched instruction on jump
    nop = 0x00000013
    inst' = mux (isJust <$> jump) (pure nop) inst

    -- IF/ID regs:
    idPC   = regEn def en pc
    idInst = regEn nop en inst'

pipelineDecode :: HiddenClockResetEnable dom
               => Signal dom (PC, Instruction)                           -- id
               -> Signal dom Bool                                        -- flush on jump
               -> Signal dom InstCtrl                                    -- mem
               -> Signal dom (Maybe (RegIndex, RegValue))                -- wb
               -> ( Signal dom (PC, InstCtrl, Vec 2 RegValue, Immediate) -- ex
                  , Signal dom Bool )                                    -- stall?
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
                -> Signal dom RegValue                                  -- wb
                -> ( Signal dom (PC, InstCtrl, Vec 2 AluOpand, AluResult, RegValue) -- mem
                   , Signal dom (Maybe PC) )                            -- jump?
pipelineExecute ex wbData = (bundle (memPC, memInst, memOpands, memAluRes, memRs2), jumpPC)
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

    memPC     = register def pc
    memOpands = register def opands

pipelineMemory :: HiddenClockResetEnable dom
               => IO.Device dom a
               -> Signal dom (PC, InstCtrl, Vec 2 AluOpand, AluResult, RegValue) -- mem
               -> (Signal dom (PC, InstCtrl, Vec 2 AluOpand, RegValue), Signal dom a) -- wb
pipelineMemory io mem = (bundle (wbPC, wbInst, wbOpands, wbData), out)
  where
    (pc, inst, opands, aluRes, rs2) = unbundle mem

    act = action <$> inst
    (rdata, out) = io $ instIO <$> act <*> (fromIntegral <$> aluRes) <*> rs2

    -- MEM/WB regs:
    wbAluRes = register def aluRes
    wbInst   = register def inst
    wbAct    = register Nop act

    wbPC     = register def pc
    wbOpands = register def opands

    rdata' = signExtendIO <$> wbAct <*> rdata

    -- Load data comes in the next clock cycle
    wbData = writebackMux . action <$> wbInst <*> wbAluRes <*> rdata'

cpu :: HiddenClockResetEnable dom
    => Vec 4 (MemBlob n 8)
    -> IO.Device dom a
    -> Signal dom (a, Trace)
cpu imblob io = bundle (out, trace)
  where
    -- Instruction fetch phase
    id = pipelineFetch imblob stall jump

    -- Instruction decode phase
    (ex, stall) = pipelineDecode id flush memInst wbRegData

    -- Execute phase
    (mem, jump) = pipelineExecute ex wbData
    (_, memInst, _, _, _) = unbundle mem

    flush = isJust <$> jump

    -- Memory and writeback phases
    (wb, out) = pipelineMemory io mem

    (wbPC, wbInst, wbOpands, wbData) = unbundle wb
    wbReg = dstReg <$> wbInst

    -- Wrap into a Maybe together with target register
    wbRegData = whenMaybe
              . writesBack
              . action <$> wbInst
                       <*> ((,) <$> wbReg <*> wbData)

    trace = wrapTrace <$> wbPC <*> wbInst <*> wbOpands <*> (fmap . fmap) snd wbRegData
