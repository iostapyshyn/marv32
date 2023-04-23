module Main where

import System.Environment
import System.FilePath.Posix
import System.Directory
import System.IO

import qualified Data.ByteString as B
import qualified Data.List as L

import Text.Printf
import Data.Bits
import Data.Word

takeEvery :: Int -> [a] -> [a]
takeEvery n = map snd
            . filter ((==0) . fst)
            . zip (cycle [0..n-1])

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = L.take n xs : chunksOf n (L.drop n xs)

toWord32 :: [Word8] -> Word32
toWord32 xs = fromNum $ fromIntegral <$> xs
  where fromNum (d:c:b:a:[]) = shiftL a 24 .|.
                               shiftL b 16 .|.
                               shiftL c  8 .|. d

createOneFile :: [Word8] -> FilePath -> Int -> IO ()
createOneFile bytes basename i = do
  withFile (wName i) WriteMode
    (\h -> mapM_ (\x -> hPutStrLn h $ printf "%08b" x) $ (takeEvery 4 . drop i) bytes)
  where wName i = printf "rom/%s/%d.rom" basename i

createRomFile :: FilePath -> IO ()
createRomFile fileR = do
  bytes <- B.unpack <$> B.readFile fileR
  mapM_ (createOneFile bytes (takeBaseName fileR)) [0..3]

main :: IO ()
main = do [fileR] <- getArgs
          let basename = takeBaseName fileR
          createDirectoryIfMissing True $ printf "rom/%s" basename
          createRomFile fileR
