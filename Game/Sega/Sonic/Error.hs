module Game.Sega.Sonic.Error (
  SonicError(..)
) where

data SonicError
  = SonicLoadError FilePath
  | SonicDecompressionError FilePath
  | SonicPaletteError FilePath
  | SonicEmptyChunksError
  | SonicEmptyCollisionIndexError
  deriving (Eq, Ord, Show)
