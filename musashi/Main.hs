module Main where

import           Control.Monad         ((>=>))
import qualified Data.ByteString       as BS
import           Data.Foldable         (traverse_)
import           Data.IORef            (writeIORef)
import           Data.Word             (Word32, Word8)
import           Foreign.C.Types       (CUInt)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Marshal.Array (copyArray)
import           Foreign.Ptr           (nullPtr)
import           Musashi

calcSineOffset :: Word32
calcSineOffset =
  0x33B6

calcSine :: CUInt -> IO CUInt
calcSine n = do
  musashiSetSubroutine calcSineOffset
  m68k_set_reg m68k_REG_D0 n
  musashiRun
  m68k_get_reg nullPtr m68k_REG_D0

main :: IO ()
main = do
  bs <- BS.readFile "../sonic2.md"
  allocaBytes 0xffffff $ \p -> do
    musashiSetRom p
    BS.useAsCStringLen bs (uncurry (copyArray p))
    musashiInit
    traverse_ (calcSine >=> print) [0..128]
