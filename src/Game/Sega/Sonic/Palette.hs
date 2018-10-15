module Game.Sega.Sonic.Palette (
  loadPalette
) where

import           Control.Lens           (over, _head)
import           Data.Array.Bounded     (BoundedArray, accumArrayBounded)
import           Data.List.Split        (chunksOf)
import           Data.Vector.Storable   (Vector, fromList)
import           Data.Word              (Word8)
import           SDL                    (V4 (..))
import           Sega.MegaDrive.Palette (BGR (..), ColorNibble (..),
                                         nibbleToByte)

fromBGR :: BGR ColorNibble -> V4 Word8
fromBGR (BGR b g r) =
  V4 (nibbleToByte r) (nibbleToByte g) (nibbleToByte b) 0xFF

makeTransparent :: V4 Word8 -> V4 Word8
makeTransparent (V4 b g r _) =
  V4 b g r 0

loadPalette :: [BGR ColorNibble] -> BoundedArray Word8 (Vector (V4 Word8))
loadPalette =
  accumArrayBounded (flip const) mempty . zip [0..3] . fmap (fromList . over _head makeTransparent) . chunksOf 0x10 . fmap fromBGR
