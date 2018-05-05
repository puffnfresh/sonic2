module Game.Sega.Sonic.Game (
  Game(..)
, gameRenderer
, gameCamera
, cameraX
, cameraY
) where

import           Control.Lens
import           Foreign.C.Types
import           SDL

data Game
  = Game Renderer (V2 CInt)

gameCamera :: Lens' Game (V2 CInt)
gameCamera =
  lens f g
  where
    f (Game _ a) =
      a
    g (Game a _) b =
      Game a b

gameRenderer :: Lens' Game Renderer
gameRenderer =
  lens f g
  where
    f (Game a _) =
      a
    g (Game _ b) a =
      Game a b

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
