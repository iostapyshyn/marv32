module Utils.Files
  ( getBank
  , takeEvery
  , chunksOf
  , toWord32 ) where

import Prelude

import qualified Clash.Prelude as P

import qualified Data.ByteString as B
import qualified Data.List as L

import Data.Word
import Data.Bits

import Language.Haskell.TH

takeEvery :: Int -> [a] -> [a]
takeEvery n = L.map snd
            . L.filter ((==0) . fst)
            . L.zip (L.cycle [0..n-1])

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = L.take n xs : chunksOf n (L.drop n xs)

toWord32 :: [Word8] -> Word32
toWord32 xs = fromNum $ fromIntegral <$> xs
  where fromNum (d:c:b:a:[]) = shiftL a 24 .|.
                               shiftL b 16 .|.
                               shiftL c  8 .|. d

-- vecE :: Quote m => [m Exp] -> m Exp
-- vecE [] = [| P.Nil |]
-- vecE (x:xs) = [| (P.:>) $x $(vecE xs) |]

getBank :: FilePath -> Int -> Q Exp
getBank file i = (runIO bankIO) >>= (P.memBlobTH Nothing)
  where bankIO = do bytes <- B.unpack <$> B.readFile file
                    return $ takeEvery 4 . L.drop i $ bytes
