module Game.Sega.Sonic.Chunks (
  loadChunks
) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Array             (Array, listArray, (!))
import           Data.Bits              (shiftL, (.&.), (.|.))
import           Data.List.Split        (chunksOf)
import           Data.Word              (Word16, Word8)
import           Debug.Trace
import           SDL

word16s :: [Word8] -> [Word16]
word16s (a:b:cs) =
  (shiftL (fromIntegral a) 8 .|. fromIntegral b) : word16s cs
word16s _ =
  []

loadChunk :: (MonadIO m) => Renderer -> Array Word16 Texture -> [[Word8]] -> m Texture
loadChunk renderer blocks s = do
  texture <- createTexture renderer ABGR8888 TextureAccessTarget 0x80
  rendererRenderTarget renderer $= Just texture
  ifor_ s $ \y row ->
    ifor_ (word16s row) $ \x i ->
      let
        rectangle =
          Rectangle (P (V2 (fromIntegral x * 0x10) (fromIntegral y * 0x10))) 0x10
      in copy renderer (blocks ! (i .&. 0x3FF)) Nothing (Just rectangle)
  pure texture

loadChunks :: (MonadIO m) => Renderer -> Array Word16 Texture -> [Word8] -> m (Array Word8 Texture)
loadChunks renderer blocks =
  fmap (listArray (0, 0xFF)) . traverse (loadChunk renderer blocks . chunksOf 0x10) . chunksOf 0x80
