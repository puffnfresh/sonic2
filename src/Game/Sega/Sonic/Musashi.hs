module Game.Sega.Sonic.Musashi where

import           Control.Monad         ((>=>))
import qualified Data.ByteString       as BS
import           Data.Foldable         (traverse_)
import           Data.Int              (Int16)
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

calcSine :: Word8 -> IO (Int16, Int16)
calcSine n = do
  musashiSetSubroutine calcSineOffset
  m68k_set_reg m68k_REG_D0 (fromIntegral n)
  musashiRun
  d0 <- m68k_get_reg nullPtr m68k_REG_D0
  d1 <- m68k_get_reg nullPtr m68k_REG_D1
  pure (fromIntegral d0, fromIntegral d1)
