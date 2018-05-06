module Game.Sega.Sonic.Game (
  Game(..)
, HasRenderer(..)
, HasCamera(..)
, HasRom(..)
, sliceRom
, cameraX
, cameraY
) where

import           Control.Lens
import qualified Data.ByteString as BS
import           Control.Monad.Reader
import           Foreign.C.Types
import Game.Sega.Sonic.Offsets
import           SDL

data Game
  = Game Renderer (V2 CInt) BS.ByteString

class HasRenderer a where
  renderer :: Lens' a Renderer

instance HasRenderer Game where
  renderer =
    lens f g
    where
      f (Game a _ _) =
        a
      g (Game _ b c) a =
        Game a b c

class HasCamera a where
  camera :: Lens' a (V2 CInt)

instance HasCamera Game where
  camera =
    lens f g
    where
      f (Game _ a _) =
        a
      g (Game a _ c) b =
        Game a b c

class HasRom a where
  rom :: Lens' a BS.ByteString

instance HasRom Game where
  rom =
    lens f g
    where
      f (Game _ _ a) =
        a
      g (Game a b _) c =
        Game a b c

sliceRom :: (HasRom g, MonadReader g m) => Offset -> m BS.ByteString
sliceRom offset =
  sliceOffset offset <$> view rom

cameraX :: Lens' (V2 a) a
cameraX =
  lens f g
  where
    f (V2 a _) =
      a
    g (V2 _ b) a =
      V2 a b

cameraY :: Lens' (V2 a) a
cameraY =
  lens f g
  where
    f (V2 _ a) =
      a
    g (V2 a _) b =
      V2 a b
