{-# LANGUAGE MagicHash #-}

module CPU.Memory
  ( instMem
  , dataMem
  ) where

import Clash.Prelude

import CPU.Types
import CPU.Decode

import Utils

concatMaybeVec :: (KnownNat n, KnownNat m)
               => Bool
               -> Vec n (Maybe (BitVector m))
               -> BitVector (n * m)
concatMaybeVec sext v = extend sext $ foldr go (0, 0) v
  where go :: (KnownNat n, KnownNat m)
           => Maybe (BitVector m)
           -> (Int, BitVector (n * m))
           -> (Int, BitVector (n * m))
        go Nothing acc           = acc
        go (Just (x)) (pos, acc) = ( pos+8
                                   , acc .|. shiftL (resize x) pos )
        extend :: (KnownNat n)
               => Bool
               -> (Int, BitVector (n))
               -> BitVector n
        extend False (_,   v) = v
        extend True  (len, v) = let bv = reverse $ bv2v v
                                    sign = bv !! (len-1)
                                    fn i x
                                      | fromIntegral i >= len = sign
                                      | otherwise             = x
                                in v2bv . reverse $ imap fn bv

instMem :: Vec 4 (MemBlob n 8) -> PC -> InstRaw
instMem blobs pc = (concatBitVector#) roms
  where index = shiftR pc 2
        roms = (asyncRomBlob (blobs !! 3) index :>
                asyncRomBlob (blobs !! 2) index :>
                asyncRomBlob (blobs !! 1) index :>
                asyncRomBlob (blobs !! 0) index :> Nil)

-- TODO: Unaligned accesses are possible
dataMem :: HiddenClockResetEnable dom
        => Vec 4 (MemBlob n 8)
        -> Signal dom MDetails
        -> Signal dom (Maybe MWordS)
        -> Signal dom MAddr
        -> Signal dom MWordS
dataMem blobs details wdata addr = go
  where
    (width, sext) = unbundle details
    disableBytes :: MWidth -> MAddr -> Index 4 -> b -> Maybe b
    disableBytes width addr i x = whenMaybe x
                                . byteEnable
                                . fromIntegral $ i
      where byteEnable i = case width of
              Word -> case addr .&. 0b11 of
                0 -> True
              Half -> case addr .&. 0b01 of
                0 -> i == (addr .&. 0b10) || i == (addr .&. 0b10 + 1)
              Byte -> fromIntegral (addr .&. 0b11) == i

    addrs :: MAddr -> Vec 4 (MAddr)
    addrs a = repeat $ shiftR a 2

    wdatas :: MWidth -> Maybe MWordS -> MAddr -> Vec 4 (Maybe (BitVector 8))
    wdatas _     (Nothing)    _    = repeat Nothing
    wdatas width (Just wdata) addr = imap (disableBytes width addr) (permuteData)
      where wdataVec = (slice d31 d24 wdata :> slice d23 d16 wdata :>
                        slice d15  d8 wdata :> slice  d7  d0 wdata :> Nil)
            permuteData = case width of
              Word -> wdataVec
              Half -> backpermute wdataVec (2:>3:>2:>3:>Nil)
              Byte -> backpermute wdataVec (3:>3:>3:>3:>Nil)

    joinData :: MWidth -> Bool -> MAddr -> Vec 4 (BitVector 8) -> MWordS
    joinData width sext addr rdatas = unpack
                                    . concatMaybeVec sext
                                    . imap (disableBytes width addr)
                                    $ rdatas

    -- read data comes in the next clock cycle
    width' = register Word width
    sext' = register False sext
    addr' = register 0 addr

    go = joinData <$> width' <*> sext' <*> addr' <*> go'
      where go' = ramsB (addrs <$> addr) (wdatas <$> width <*> wdata <*> addr)

    rams :: HiddenClockResetEnable dom
         => Vec 4 (Signal dom (MAddr))
         -> Vec 4 (Signal dom (Maybe (BitVector 8)))
         -> Vec 4 (Signal dom (BitVector 8))
    rams addrs wdatas = (blockRamBlob (blobs !! 3) (addrs !! 0) (wports !! 0) :>
                         blockRamBlob (blobs !! 2) (addrs !! 1) (wports !! 1) :>
                         blockRamBlob (blobs !! 1) (addrs !! 2) (wports !! 2) :>
                         blockRamBlob (blobs !! 0) (addrs !! 3) (wports !! 3) :> Nil)
      where wports = zipWith (\addr wdata -> tuplify <$> addr <*> wdata) addrs wdatas
            tuplify addr' = fmap ((,) addr')

    ramsB addr wdata = bundle $ rams (unbundle addr) (unbundle wdata)
