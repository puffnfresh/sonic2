module Game.Sega.Sonic.SpriteMappings (
  SpriteMapping(..)
, loadMappings
) where

import           Data.Bits          (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString    as BS
import           Data.List.Split    (chunksOf)
import           Data.Word          (Word16, Word8)

import Debug.Trace

data SpriteMapping
  = SpriteMapping Word8 Word8 Word16

loadMapping :: [Word8] -> SpriteMapping
loadMapping (_:_:_:_:y:x:s:_:[]) =
  let
    width =
      (s .&. 0xC) `shiftR` 2
    height =
      s .&. 0x3
    patternIndex =
      shiftL (fromIntegral x) 8 .|. fromIntegral y
  in traceShow (width, height, patternIndex :: Word16) $ SpriteMapping width height patternIndex

loadMappings :: BS.ByteString -> [SpriteMapping]
loadMappings =
  fmap loadMapping . chunksOf 8 . BS.unpack
