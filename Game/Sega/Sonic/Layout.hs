module Game.Sega.Sonic.Layout (
  loadLayout
) where

import           Control.Lens    (folded, ifiltered, toListOf)
import           Data.Array      (Array, (!))
import           Data.List.Split (chunksOf)
import           Data.Word       (Word8)
import           SDL             (Texture)

ifilter :: Foldable f => (Int -> a -> Bool) -> f a -> [a]
ifilter f =
  toListOf (folded . ifiltered f)

loadLayout :: Array Word8 Texture -> [Word8] -> [[Texture]]
loadLayout chunks =
  fmap (fmap (chunks !)) . ifilter (const . even) . chunksOf 0x80
