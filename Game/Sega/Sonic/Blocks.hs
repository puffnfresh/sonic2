module Game.Sega.Sonic.Blocks (
  loadBlocks
) where

import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Array.Bounded      (BoundedArray, listArrayFill)
import           Data.Bits
import qualified Data.ByteString         as BS
import           Data.List.Split         (chunksOf)
import           Data.Vector.Storable    (Vector)
import           Data.Word               (Word16, Word8)
import           Game.Sega.Sonic.Sprites (copySpriteTile)
import           SDL                     hiding (Vector)

word16s :: [Word8] -> [Word16]
word16s (a:b:cs) =
  (shiftL (fromIntegral a) 8 .|. fromIntegral b) : word16s cs
word16s _ =
  []

loadBlock :: (MonadIO m) => Renderer -> BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> [Word16] -> m Texture
loadBlock renderer palette tiles c = do
  texture <- createTexture renderer ABGR8888 TextureAccessTarget $ V2 0x10 0x10
  rendererRenderTarget renderer $= Just texture
  copySpriteTile renderer palette tiles (c !! 0) $ V2 0 0
  copySpriteTile renderer palette tiles (c !! 1) $ V2 8 0
  copySpriteTile renderer palette tiles (c !! 2) $ V2 0 8
  copySpriteTile renderer palette tiles (c !! 3) $ V2 8 8
  pure texture

emptyTexture :: (MonadIO m) => Renderer -> m Texture
emptyTexture renderer = do
  surface <- createRGBSurface (V2 0x10 0x10) ABGR8888
  surfaceFillRect surface Nothing (V4 0xFF 0xFF 0x00 0xFF)
  createTextureFromSurface renderer surface

loadBlocks :: (MonadIO m) => Renderer -> BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> BS.ByteString -> m (BoundedArray Word16 Texture)
loadBlocks renderer palette tiles c = do
  e <- emptyTexture renderer
  fmap (listArrayFill e) . traverse (loadBlock renderer palette tiles) . chunksOf 4 . word16s $ BS.unpack c
