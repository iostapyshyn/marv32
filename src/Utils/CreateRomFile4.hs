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

import Utils.Files

createOneFile :: [Word8] -> FilePath -> Int -> IO ()
createOneFile bytes basename i = do
  withFile (wName i) WriteMode
    (\h -> mapM_ (hPutStrLn h . printf "%08b") $ (takeEvery 4 . drop i) bytes)
  where wName = printf "rom/%s/%d.rom" basename

createRomFile :: FilePath -> IO ()
createRomFile fileR = do
  bytes <- B.unpack <$> B.readFile fileR
  mapM_ (createOneFile bytes (takeBaseName fileR)) [0..3]

main :: IO ()
main = do [fileR] <- getArgs
          let basename = takeBaseName fileR
          createDirectoryIfMissing True $ printf "rom/%s" basename
          createRomFile fileR
