module Game.Sega.Sonic.SpriteMappings (
  PatternIndex(..)
, SpriteFrame(..)
, SpriteMapping(..)
, DynamicPatternLoadCueFrame(..)
, DynamicPatternLoadCue(..)
, loadMappings
, loadDynamicPatternLoadCues
) where

import           Control.Lens
import           Data.Bits            (shiftR, (.&.))
import qualified Data.ByteString      as BS
import           Data.ByteString.Lens (unpackedBytes)
import           Data.Halves          (collectHalves)
import           Data.Int             (Int16, Int8)
import           Data.List.Split      (chunksOf)
import           Data.Word            (Word16, Word8)

newtype PatternIndex
  = PatternIndex Word16
  deriving (Eq, Ord, Show)

data SpriteFrame
  = SpriteFrame [SpriteMapping PatternIndex]
  deriving (Eq, Ord, Show)

data SpriteMapping a
  = SpriteMapping Int16 Int8 Word8 Word8 a
  deriving (Eq, Ord, Show)

data DynamicPatternLoadCueFrame
  = DynamicPatternLoadCueFrame [DynamicPatternLoadCue]
  deriving (Eq, Ord, Show)

data DynamicPatternLoadCue
  = DynamicPatternLoadCue Word8 Word16
  deriving (Eq, Ord, Show)

loadMapping :: [Word16] -> SpriteMapping PatternIndex
loadMapping (s:c:_:l:[]) =
  let
    t =
      fromIntegral $ s `shiftR` 8
    w =
      fromIntegral $ (s .&. 0xC) `shiftR` 2
    h =
      fromIntegral $ s .&. 0x3
  in SpriteMapping (fromIntegral l) t w h $ PatternIndex c
loadMapping _ =
  SpriteMapping 0 0 0 0 $ PatternIndex 0

countedWords :: (Int -> Int) -> [Word16] -> [[Word16]]
countedWords f (x:xs) =
  let (a, b) = splitAt (f $ fromIntegral x) xs
  in a : countedWords f b
countedWords _ [] =
  []

ignoreOffsets :: BS.ByteString -> [Word16]
ignoreOffsets =
  dropWhile (/= 0) . view (unpackedBytes . collectHalves)

loadMappings :: BS.ByteString -> [SpriteFrame]
loadMappings =
  fmap (SpriteFrame . fmap loadMapping . chunksOf 4) . countedWords (* 4) . ignoreOffsets

loadDynamicPatternLoadCue :: Word16 -> DynamicPatternLoadCue
loadDynamicPatternLoadCue w =
  let
    count =
      fromIntegral (w `shiftR` 12)
    offset =
      w .&. 0xFFF
  in DynamicPatternLoadCue count offset

loadDynamicPatternLoadCues :: BS.ByteString -> [DynamicPatternLoadCueFrame]
loadDynamicPatternLoadCues =
  fmap (DynamicPatternLoadCueFrame . fmap loadDynamicPatternLoadCue) . countedWords id . ignoreOffsets
