{-# LANGUAGE NamedFieldPuns #-}

module CPU.Trace
  ( Trace (..)
  , wrapTrace
  , prettyTrace ) where

import Clash.Prelude

import CPU.Machine
import CPU.Instruction

import Text.Printf
import qualified Data.List as L

data Trace = Trace { pc :: PC
                   , ctrl :: InstCtrl
                   , opands :: Vec 2 AluOpand
                   , wb :: Maybe RegValue }
  deriving (Show, Generic, NFDataX)

wrapTrace :: PC -> InstCtrl -> Vec 2 AluOpand -> Maybe RegValue
          -> Trace
wrapTrace pc ctrl opands wb = Trace { pc = pc
                                    , ctrl = ctrl
                                    , opands = opands
                                    , wb = wb }

prettyTrace :: Trace -> String
prettyTrace Trace { pc, ctrl, opands, wb } =
  L.concat $ [ printf "%04x: " pc, show ctrl, "\n"
             , printf "%04x: " pc, show . toList $ opands, " -> ", show wb, "\n\n"]
