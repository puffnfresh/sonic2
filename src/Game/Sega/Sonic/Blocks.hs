{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Blocks (
  loadBlocks
) where

import           Control.Lens            (view)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Reader    (MonadReader)
import           Data.Array.Bounded      (BoundedArray, listArrayFill)
import           Data.Bits
import qualified Data.ByteString         as BS
import           Data.List.Split         (chunksOf)
import           Data.Vector.Storable    (Vector)
import           Data.Word               (Word16, Word8)
import           Game.Sega.Sonic.Game    (HasRenderer (..))
import           Game.Sega.Sonic.Sprites (copySpriteTile)
import           SDL                     hiding (Vector)

word16s :: [Word8] -> [Word16]
word16s (a:b:cs) =
  (shiftL (fromIntegral a) 8 .|. fromIntegral b) : word16s cs
word16s _ =
  []

loadBlock :: (HasRenderer g, MonadReader g m, MonadIO m) => BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> [Word16] -> m Texture
loadBlock palette tiles c = do
  r <- view renderer
  texture <- createTexture r ABGR8888 TextureAccessTarget $ V2 0x10 0x10
  rendererRenderTarget r $= Just texture
  copySpriteTile palette tiles (c !! 0) $ V2 0 0
  copySpriteTile palette tiles (c !! 1) $ V2 8 0
  copySpriteTile palette tiles (c !! 2) $ V2 0 8
  copySpriteTile palette tiles (c !! 3) $ V2 8 8
  pure texture

emptyTexture :: (HasRenderer g, MonadReader g m, MonadIO m) => m Texture
emptyTexture = do
  surface <- createRGBSurface (V2 0x10 0x10) ABGR8888
  surfaceFillRect surface Nothing (V4 0xFF 0xFF 0x00 0xFF)
  r <- view renderer
  createTextureFromSurface r surface

loadBlocks :: (HasRenderer g, MonadReader g m, MonadIO m) => BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> BS.ByteString -> m (BoundedArray Word16 Texture)
loadBlocks palette tiles c = do
  e <- emptyTexture
  fmap (listArrayFill e) . traverse (loadBlock palette tiles) . chunksOf 4 . word16s $ BS.unpack c
