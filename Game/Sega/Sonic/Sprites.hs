module Game.Sega.Sonic.Sprites (
  copySpriteTile
, copySpriteMapping
) where

import           Control.Applicative            (liftA2)
import           Control.Lens                   (ifor_)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Array.Bounded
import           Data.Bits                      (shiftR, testBit, (.&.))
import           Data.Foldable                  (for_)
import           Data.Vector.Storable           (Vector)
import           Data.Word                      (Word16, Word8)
import           Foreign.C.Types                (CInt)
import           Game.Sega.Sonic.SpriteMappings (PatternIndex (..),
                                                 SpriteMapping (..))
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
  maybeTilePalette <- formatPalette format
  for_ maybeTilePalette $ \tilePalette ->
    setPaletteColors tilePalette (palette ! paletteIndex) 0
  tileTexture <- createTextureFromSurface renderer tileSurface
  copyEx renderer tileTexture Nothing (Just $ Rectangle (P v) 8) 0 Nothing $ V2 flipX flipY
  destroyTexture tileTexture

copySpriteMapping :: (MonadIO m) => Renderer -> BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> SpriteMapping PatternIndex -> m (SpriteMapping Texture)
copySpriteMapping renderer palette tiles (SpriteMapping top left width height (PatternIndex patternIndex)) = do
  let
    width' =
      8 * (width + 1)
    height' =
      8 * (height + 1)
  texture <- createTexture renderer ABGR8888 TextureAccessTarget $ V2 (fromIntegral width') (fromIntegral height')
  rendererRenderTarget renderer $= Just texture
  ifor_ (liftA2 (,) [0..width] [0..height]) $ \i (x, y) ->
    let
      patternIndex' =
        patternIndex + fromIntegral i
      position =
        V2 (fromIntegral x * 8) (fromIntegral y * 8)
    in copySpriteTile renderer palette tiles patternIndex' position
  pure $ SpriteMapping top left width' height' texture
