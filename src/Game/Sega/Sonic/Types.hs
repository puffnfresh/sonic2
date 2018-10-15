module Game.Sega.Sonic.Types (
  HasPosition(..)
) where

import           Control.Lens    (Lens')
import           Foreign.C.Types (CInt)
import           SDL             (V2)

class HasPosition a where
  position :: Lens' a (V2 CInt)
