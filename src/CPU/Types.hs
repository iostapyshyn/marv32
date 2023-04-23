module CPU.Types
  ( MWordU
  , MWordS
  , PC
  ) where

import Clash.Prelude

type MWordU   = Unsigned 32 -- Machine word (Unsigned)
type MWordS   = Signed 32   -- Machine word (Signed)
type PC       = Unsigned 32 -- Program counter
