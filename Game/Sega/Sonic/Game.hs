{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Game (
  Game(..)
, HasRenderer(..)
, HasCamera(..)
, HasRom(..)
, HasPlayer(..)
, sliceRom
, cameraX
, cameraY
) where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.ByteString         as BS
import           Foreign.C.Types
import           Game.Sega.Sonic.Offsets as Offsets
import           Game.Sega.Sonic.Player
import           SDL

data Game
  = Game Renderer (V2 CInt) BS.ByteString Player

class HasRenderer a where
  renderer :: Lens' a Renderer

instance HasRenderer Game where
  renderer =
    lens f g
    where
      f (Game a _ _ _) =
        a
      g (Game _ b c d) a =
        Game a b c d

class HasCamera a where
  camera :: Lens' a (V2 CInt)

instance HasCamera Game where
  camera =
    lens f g
    where
      f (Game _ a _ _) =
        a
      g (Game a _ c d) b =
        Game a b c d

class HasRom a where
  rom :: Lens' a BS.ByteString

instance HasRom Game where
  rom =
    lens f g
    where
      f (Game _ _ a _) =
        a
      g (Game a b _ d) c =
        Game a b c d

class HasPlayer a where
  player :: Lens' a Player

instance HasPlayer Game where
  player =
    lens f g
    where
      f (Game _ _ _ a) =
        a
      g (Game a b c _) d =
        Game a b c d

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
