module Game.Sega.Sonic.Sprites (
  SizedTexture(..)
, copySpriteTile
, copySpriteMapping
) where

import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Array.Bounded             (BoundedArray, listArrayFill,
                                                 (!))
import           Data.Bits                      (shiftR, testBit, (.&.))
import qualified Data.ByteString                as BS
import           Data.Foldable                  (for_)
import           Data.List.Split                (chunksOf)
import           Data.Vector.Storable           (Vector)
import           Data.Word                      (Word16, Word8)
import           Foreign.C.Types                (CInt)
import           Foreign.Ptr                    (Ptr, castPtr)
import           Foreign.Storable               (pokeElemOff)
import           Game.Sega.Sonic.SpriteMappings (SpriteMapping (..))
import           SDL                            hiding (Vector)

copySpriteTile :: (MonadIO m) => Renderer -> BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> Word16 -> V2 CInt -> m ()
copySpriteTile renderer palette sprite c v = do
  let
    tileSurface =
      sprite ! (c .&. 0x7FF)
    paletteIndex =
      fromIntegral (c `shiftR` 13 .&. 0x3)
    flipY =
      testBit c 0xC
    flipX =
      testBit c 0xB
  format <- surfaceFormat tileSurface
  maybeCellPalette <- formatPalette format
  for_ maybeCellPalette $ \cellPalette ->
    setPaletteColors cellPalette (palette ! paletteIndex) 0
  cellTexture <- createTextureFromSurface renderer tileSurface
  copyEx renderer cellTexture Nothing (Just $ Rectangle (P v) 8) 0 Nothing $ V2 flipX flipY
  destroyTexture cellTexture

data SizedTexture
  = SizedTexture Word8 Word8 Texture

copySpriteMapping :: (MonadIO m) => Renderer -> BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> SpriteMapping -> m SizedTexture
copySpriteMapping renderer palette tiles (SpriteMapping width height patternIndex) = do
  let
    width' =
      8 * (width + 1)
    height' =
      8 * (height + 1)
  texture <- createTexture renderer ABGR8888 TextureAccessTarget $ V2 (fromIntegral width') (fromIntegral height')
  rendererRenderTarget renderer $= Just texture
  for_ [0..height] $ \y ->
    for_ [0..width] $ \x ->
      let
        patternIndex' =
          patternIndex + fromIntegral y + (fromIntegral x * (fromIntegral height + 1))
      in copySpriteTile renderer palette tiles patternIndex' $ V2 (fromIntegral x * 8) (fromIntegral y * 8)
  pure $ SizedTexture width' height' texture
