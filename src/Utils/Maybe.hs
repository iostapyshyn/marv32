module Utils.Maybe
  ( whenMaybe
  , gatherMaybe
  ) where

import Clash.Prelude

whenMaybe :: Bool -> a -> Maybe a
whenMaybe False _ = Nothing
whenMaybe True  a = Just a

gatherMaybe :: (Enum i, KnownNat n)
            => Vec n a          -- ^ Source vector
            -> Vec m (Maybe i)  -- ^ Source index mapping
            -> Vec m (Maybe a)
gatherMaybe xs = map (fmap (xs!!))
