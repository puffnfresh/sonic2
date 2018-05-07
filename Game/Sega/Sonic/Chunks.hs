{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Chunks (
  loadChunkTexture
, loadChunks
, ChunkBlock(..)
) where

import           Control.Lens
import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader)
import           Data.Array.Bounded
import           Data.Bits
import qualified Data.ByteString        as BS
import           Data.Halves            (collectHalves)
import           Data.List.NonEmpty     (nonEmpty)
import           Data.List.Split        (chunksOf)
import           Data.Word              (Word16, Word8)
import           Game.Sega.Sonic.Error
import           Game.Sega.Sonic.Game   (HasRenderer (..))
import           SDL

import Debug.Trace

loadChunkTexture :: (HasRenderer g, MonadReader g m, MonadIO m) => BoundedArray Word16 Texture -> Chunk -> m Texture
loadChunkTexture blocks chunkBlocks = do
  r <- view renderer
  texture <- createTexture r ABGR8888 TextureAccessTarget 0x80
  rendererRenderTarget r $= Just texture
  ifor_ (unboundedArray chunkBlocks) $ \i (ChunkBlock blockIndex flipX flipY) ->
    let
      (y, x) =
        i `divMod` 8
      rectangle =
        Rectangle (P (V2 (fromIntegral x * 0x10) (fromIntegral y * 0x10))) 0x10
    in copyEx r (blocks ! blockIndex) Nothing (Just rectangle) 0 Nothing (V2 flipX flipY)
  pure texture

data ChunkBlock
  = ChunkBlock Word16 Bool Bool

emptyChunkBlock :: ChunkBlock
emptyChunkBlock =
  ChunkBlock 0 False False

type Chunk
  = BoundedArray Word8 ChunkBlock

loadChunkBlock :: Word16 -> ChunkBlock
loadChunkBlock i =
  ChunkBlock (i .&. 0x3FF) (testBit i 0xA) (testBit i 0xB)

loadChunk :: [Word16] -> Chunk
loadChunk ws =
  listArrayFill emptyChunkBlock $ loadChunkBlock <$> ws

emptyChunk :: Chunk
emptyChunk =
  listArrayFill emptyChunkBlock []

loadChunks :: BS.ByteString -> BoundedArray Word8 Chunk
loadChunks = do
  listArrayFill emptyChunk . fmap loadChunk . chunksOf 0x40 . view collectHalves . BS.unpack
