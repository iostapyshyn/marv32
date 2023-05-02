module Utils
  ( whenMaybe
  , gatherMaybe
  , liftA4
  ) where

import Clash.Prelude

whenMaybe :: Bool -> a -> Maybe a
whenMaybe False _ = Nothing
whenMaybe True  a = Just a

gatherMaybe :: (Enum i, KnownNat n)
            => Vec n a          -- ^ Source vector
            -> Vec m (Maybe i)  -- ^ Source index mapping
            -> Vec m (Maybe a)
gatherMaybe xs = map (\x -> fmap (xs!!) x)

liftA4 :: Applicative f
       =>  (a ->   b ->   c ->   d ->   e)
       -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
