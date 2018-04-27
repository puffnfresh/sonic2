module Game.Sega.Sonic.Layout (
  loadLayout
) where

import           Control.Lens    (folded, ifiltered, toListOf)
import           Data.Array      (Array, (!))
import qualified Data.ByteString as BS
import           Data.List.Split (chunksOf)
import           Data.Word       (Word8)
import           SDL             (Texture)

ifilter :: Foldable f => (Int -> a -> Bool) -> f a -> [a]
ifilter f =
  toListOf (folded . ifiltered f)

loadLayout :: Array Word8 Texture -> BS.ByteString -> [[Texture]]
loadLayout chunks =
  ifilter (const . even) . chunksOf 0x80 . fmap (chunks !) . BS.unpack
