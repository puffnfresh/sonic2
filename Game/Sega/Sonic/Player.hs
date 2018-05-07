module Game.Sega.Sonic.Player (
  Player(..)
, HasPosition(..)
) where

import           Control.Lens
import           Foreign.C.Types
import           Game.Sega.Sonic.Types
import           SDL

data Player
  = Player (V2 CInt) (V2 CInt)

instance HasPosition Player where
  position =
    lens f g
    where
      f (Player a _) =
        a
      g (Player _ b) a =
        Player a b
