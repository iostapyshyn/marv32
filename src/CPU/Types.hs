module CPU.Types
  ( MWordU
  , MWordS
  , MAddr
  , PC
  ) where

import Clash.Prelude

type MWordU   = Unsigned 32 -- Machine word (Unsigned)
type MWordS   = Signed 32   -- Machine word (Signed)

type MAddr    = MWordU      -- Address line
type PC       = MAddr       -- Program counter
