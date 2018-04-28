module Game.Sega.Sonic.Blocks (
  loadBlocks
) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Array             (Array, listArray, (!))
import           Data.Bits
import qualified Data.ByteString        as BS
import           Data.Foldable          (for_)
import           Data.List.Split        (chunksOf)
import           Data.Semigroup         ((<>))
import           Data.Vector.Storable   (Vector)
import           Data.Word              (Word16, Word8)
import           Foreign.C.Types        (CInt)
import           SDL                    hiding (Vector)

word16s :: [Word8] -> [Word16]
word16s (a:b:cs) =
  (shiftL (fromIntegral a) 8 .|. fromIntegral b) : word16s cs
word16s _ =
  []

copyCell :: (MonadIO m) => Renderer -> Array Word8 (Vector (V4 Word8)) -> Array Word16 Surface -> Word16 -> V2 CInt -> m ()
copyCell renderer palette cells c v = do
  let
    cellSurface =
      cells ! (c .&. 0x7FF)
    paletteIndex =
      fromIntegral (c `shiftR` 13 .&. 0x3)
    flipY =
      testBit c 0xC
    flipX =
      testBit c 0xB
  format <- surfaceFormat cellSurface
  maybeCellPalette <- formatPalette format
  for_ maybeCellPalette $ \cellPalette ->
    setPaletteColors cellPalette (palette ! paletteIndex) 0
  cellTexture <- createTextureFromSurface renderer cellSurface
  copyEx renderer cellTexture Nothing (Just $ Rectangle (P v) 8) 0 Nothing $ V2 flipX flipY
  destroyTexture cellTexture

loadBlock :: (MonadIO m) => Renderer -> Array Word8 (Vector (V4 Word8)) -> Array Word16 Surface -> [Word16] -> m Texture
loadBlock renderer palette cells c = do
  texture <- createTexture renderer ABGR8888 TextureAccessTarget $ V2 0x10 0x10
  rendererRenderTarget renderer $= Just texture
  copyCell renderer palette cells (c !! 0) $ V2 0 0
  copyCell renderer palette cells (c !! 1) $ V2 8 0
  copyCell renderer palette cells (c !! 2) $ V2 0 8
  copyCell renderer palette cells (c !! 3) $ V2 8 8
  pure texture

emptyTexture :: (MonadIO m) => Renderer -> m Texture
emptyTexture renderer = do
  surface <- createRGBSurface (V2 0x10 0x10) ABGR8888
  surfaceFillRect surface Nothing (V4 0xFF 0xFF 0x00 0xFF)
  createTextureFromSurface renderer surface

loadBlocks :: (MonadIO m) => Renderer -> Array Word8 (Vector (V4 Word8)) -> Array Word16 Surface -> BS.ByteString -> m (Array Word16 Texture)
loadBlocks renderer palette cells c = do
  e <- emptyTexture renderer
  fmap (listArray (0, 0x3FF) . (<> repeat e)) . traverse (loadBlock renderer palette cells) . chunksOf 4 . word16s $ BS.unpack c
