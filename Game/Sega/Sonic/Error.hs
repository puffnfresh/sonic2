module Game.Sega.Sonic.Error (
  SonicError(..)
) where

import           Game.Sega.Sonic.Offsets (Offset)

data SonicError
  = SonicLoadError Offset
  | SonicDecompressionError Offset
  | SonicPaletteError Offset
  | SonicEmptyChunksError
  | SonicEmptyCollisionIndexError
  deriving (Eq, Ord, Show)
