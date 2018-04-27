module Game.Sega.Sonic.Palette (
  loadPalette
) where

import           Data.Array             (Array, listArray)
import qualified Data.ByteString        as BS
import           Data.List.Split        (chunksOf)
import           Data.Semigroup         ((<>))
import           Data.Vector.Storable   (Vector, fromList)
import           Data.Word              (Word8)
import           SDL                    (V4 (..))
import           Sega.MegaDrive.Palette (BGR (..), ColorNibble, nibbleToByte)

fromBGR :: BGR ColorNibble -> V4 Word8
fromBGR (BGR b g r) =
  V4 (nibbleToByte r) (nibbleToByte g) (nibbleToByte b) 0xFF

loadPalette :: [BGR ColorNibble] -> Array Word8 (Vector (V4 Word8))
loadPalette = do
  listArray (0, 3) . fmap fromList . chunksOf 0x10 . (<> replicate 0x20 0xFF000000) . (replicate 0x10 0xFF000000 <>) . fmap fromBGR
