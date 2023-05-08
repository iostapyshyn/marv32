module Periph.IO
  ( Access (..)
  , Device
  ) where

import Clash.Prelude

import CPU.Machine

data Access =
  Access { width :: Unsigned 2
         , addr  :: MAddr
         , wdata :: Maybe MWordS }
  deriving (Show, Generic, NFDataX)

type Device dom a =
  Signal dom (Maybe Access) -> (Signal dom MWordS, Signal dom a)
