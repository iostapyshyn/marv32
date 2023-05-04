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

type Device dom =
  Signal dom (Maybe Access) -> Signal dom MWordS
