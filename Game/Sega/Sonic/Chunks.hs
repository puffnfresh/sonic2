{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Chunks (
  loadChunks
) where

import           Control.Lens
import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Array.Bounded     (BoundedArray, listArrayCycle, (!))
import           Data.Bits
import qualified Data.ByteString        as BS
import           Data.List.NonEmpty     (nonEmpty)
import           Data.List.Split        (chunksOf)
import           Data.Word              (Word16, Word8)
import           Game.Sega.Sonic.Error
import           SDL

word16s :: [Word8] -> [Word16]
word16s (a:b:cs) =
  (shiftL (fromIntegral a) 8 .|. fromIntegral b) : word16s cs
word16s _ =
  []

loadChunk :: (MonadIO m) => Renderer -> BoundedArray Word16 Texture -> [[Word8]] -> m Texture
loadChunk renderer blocks s = do
  texture <- createTexture renderer ABGR8888 TextureAccessTarget 0x80
  rendererRenderTarget renderer $= Just texture
  ifor_ s $ \y row ->
    ifor_ (word16s row) $ \x i ->
      let
        rectangle =
          Rectangle (P (V2 (fromIntegral x * 0x10) (fromIntegral y * 0x10))) 0x10
        flipY =
          testBit i 0xB
        flipX =
          testBit i 0xA
      in copyEx renderer (blocks ! (i .&. 0x3FF)) Nothing (Just rectangle) 0 Nothing (V2 flipX flipY)
  pure texture

loadChunks :: (MonadError SonicError m, MonadIO m) => Renderer -> BoundedArray Word16 Texture -> BS.ByteString -> m (BoundedArray Word8 Texture)
loadChunks renderer blocks c = do
  xs <- maybe (throwError SonicEmptyChunksError) pure . nonEmpty . chunksOf 0x80 $ BS.unpack c
  fmap listArrayCycle $ traverse (loadChunk renderer blocks . chunksOf 0x10) xs
