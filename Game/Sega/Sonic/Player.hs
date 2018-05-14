module Game.Sega.Sonic.Player (
  Player(..)
, HasPosition(..)
, playerRadius
) where

import           Control.Lens
import           Foreign.C.Types
import           Game.Sega.Sonic.Types
import           SDL

data Player
  = Player (V2 CInt) (V2 CInt) (V2 CInt) Float Float

instance HasPosition Player where
  position =
    lens f g
    where
      f (Player a _ _ _ _) =
        a
      g (Player _ b c d e) a =
        Player a b c d e

playerVelocity :: Lens' Player (V2 CInt)
playerVelocity =
  lens f g
  where
    f (Player _ b _ _ _) =
      b
    g (Player a _ c d e) b =
      Player a b c d e

playerRadius :: Lens' Player (V2 CInt)
playerRadius =
  lens f g
  where
    f (Player _ _ c _ _) =
      c
    g (Player a b _ d e) c =
      Player a b c d e

playerTopSpeed :: Lens' Player Float
playerTopSpeed =
  lens f g
  where
    f (Player _ _ _ d _) =
      d
    g (Player a b c _ e) d =
      Player a b c d e

playerInertia :: Lens' Player Float
playerInertia =
  lens f g
  where
    f (Player _ _ _ _ e) =
      e
    g (Player a b c d _) e =
      Player a b c d e
