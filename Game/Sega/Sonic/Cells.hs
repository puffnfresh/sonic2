module Game.Sega.Sonic.Cells (
  loadCells
) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Array             (Array, listArray)
import           Data.Bits              (shiftR, (.&.))
import qualified Data.ByteString        as BS
import           Data.Foldable          (for_)
import           Data.List.Split        (chunksOf)
import           Data.Semigroup         ((<>))
import           Data.Word              (Word16, Word8)
import           Foreign.Ptr            (Ptr, castPtr)
import           Foreign.Storable       (pokeElemOff)
import           SDL

loadCell :: (MonadIO m) => [[Word8]] -> m Surface
loadCell c = do
  surface <- createRGBSurface (V2 8 8) Index8
  lockSurface surface
  ptr <- surfacePixels surface
  let ptr' = castPtr ptr :: Ptr Word8
  for_ [0..7] $ \y ->
    for_ [0..7] $ \x ->
      liftIO $ pokeElemOff ptr' ((y * 8) + x) $ c !! y !! x
  unlockSurface surface
  return surface

emptySurface :: (MonadIO m) => m Surface
emptySurface = do
  surface <- createRGBSurface (V2 8 8) ABGR8888
  surfaceFillRect surface Nothing (V4 0xFF 0x00 0xFF 0xFF)
  pure surface

splitByte :: Word8 -> [Word8]
splitByte a =
  [a `shiftR` 4, a .&. 0xF]

loadCells :: (MonadIO m) => BS.ByteString -> m (Array Word16 Surface)
loadCells c = do
  e <- emptySurface
  fmap (listArray (0, 0x7FF) . (<> repeat e)) . traverse (loadCell . chunksOf 8) . chunksOf 0x40 $ splitByte =<< BS.unpack c
