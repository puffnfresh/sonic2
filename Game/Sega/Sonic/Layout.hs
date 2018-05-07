module Game.Sega.Sonic.Layout (
  loadLayout
, mapChunkTextures
) where

import           Control.Lens       (folded, ifiltered, toListOf)
import           Data.Array.Bounded (BoundedArray, (!))
import qualified Data.ByteString    as BS
import           Data.List.Split    (chunksOf)
import           Data.Word          (Word8)
import           SDL                (Texture)

ifilter :: Foldable f => (Int -> a -> Bool) -> f a -> [a]
ifilter f =
  toListOf (folded . ifiltered f)

type ChunkIndex
  = Word8

loadLayout :: BS.ByteString -> [[ChunkIndex]]
loadLayout =
  ifilter (const . even) . chunksOf 0x80 . BS.unpack

mapChunkTextures :: BoundedArray Word8 Texture -> [[ChunkIndex]] -> [[Texture]]
mapChunkTextures chunkTextures =
  (fmap . fmap) ((chunkTextures !) . fromIntegral)
