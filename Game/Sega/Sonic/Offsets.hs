module Game.Sega.Sonic.Offsets where

import qualified Data.ByteString as BS
import           Data.Word       (Word32)

data Offset
  = Offset Word32 Word32
  deriving (Eq, Ord, Show)

paletteSonic :: Offset
paletteSonic =
  Offset 0x29E2 0x2A02

paletteEhz :: Offset
paletteEhz =
  Offset 0x2A22 0x2A82

collisionArray1 :: Offset
collisionArray1 =
  Offset 0x42E50 0x43E50

collisionEhzHtzPrimary :: Offset
collisionEhzHtzPrimary =
  Offset 0x44E50 0x44F40

layoutEhz1 :: Offset
layoutEhz1 =
  Offset 0x45AC4 0x45C84

layoutEhz2 :: Offset
layoutEhz2 =
  Offset 0x45C84 0x45E74

artSonic :: Offset
artSonic =
  Offset 0x50000 0x64320

artTails :: Offset
artTails =
  Offset 0x64320 0x6FBE0

mappingSonic :: Offset
mappingSonic =
  Offset 0x6FBE0 0x714E0

mappingTails :: Offset
mappingTails =
  Offset 0x739E2 0x7446C

animationSonicWalk :: Offset
animationSonicWalk =
  Offset 0x1B668 0x1B672

animationSonicRun :: Offset
animationSonicRun =
  Offset 0x1B666 0x1B670

animationSonicRoll :: Offset
animationSonicRoll =
  Offset 0x1B670 0x1B67A

animationSonicRoll2 :: Offset
animationSonicRoll2 =
  Offset 0x1B67A 0x1B684

animationSonicPush :: Offset
animationSonicPush =
  Offset 0x1B684 0x1B68E

animationSonicWait :: Offset
animationSonicWait =
  Offset 0x1B68E 0x1B744

animationTailsWait :: Offset
animationTailsWait =
  Offset 0x1D0A2 0x1D0E0

dplcSonic :: Offset
dplcSonic =
  Offset 0x714E0 0x71D8E

dplcTails :: Offset
dplcTails =
  Offset 0x7446C 0x74876

blockEhz :: Offset
blockEhz =
  Offset 0x94E74 0x95C24

artEhzHtz :: Offset
artEhzHtz =
  Offset 0x95C24 0x985A4

chunkEhzHtz :: Offset
chunkEhzHtz =
  Offset 0x99D34 0x9CFD4

startPosEhz1 :: Offset
startPosEhz1 =
  Offset 0xC1D0 0xC1D4

data LevelOffsets
  = LevelOffsets { levelLayoutOffset    :: Offset
                 , levelChunksOffset    :: Offset
                 , levelBlocksOffset    :: Offset
                 , levelCollisionOffset :: Offset
                 , levelPaletteOffset   :: Offset
                 , levelArtOffset       :: Offset
                 , levelStartPos        :: Offset
                 }
  deriving (Eq, Ord, Show)

ehz1 :: LevelOffsets
ehz1 =
  LevelOffsets layoutEhz1 chunkEhzHtz blockEhz collisionEhzHtzPrimary paletteEhz artEhzHtz startPosEhz1

data SpriteOffsets
  = SpriteOffsets { spriteArt     :: Offset
                  , spriteMapping :: Offset
                  , spritePalette :: Offset
                  , spriteDPLC    :: Offset
                  }
  deriving (Eq, Ord, Show)

sonicOffsets :: SpriteOffsets
sonicOffsets =
  SpriteOffsets artSonic mappingSonic paletteSonic dplcSonic

tailsOffsets :: SpriteOffsets
tailsOffsets =
  SpriteOffsets artTails mappingTails paletteSonic dplcTails

sliceOffset :: Offset -> BS.ByteString -> BS.ByteString
sliceOffset (Offset start end) =
  BS.take (fromIntegral count) . BS.drop (fromIntegral start)
  where
    count =
      end - start
