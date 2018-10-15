module Game.Sega.Sonic.Sine (
  SineData(..)
, HasSineData(..)
, calcSine
, AngleData(..)
, HasAngleData(..)
, calcAngle
) where

import           Control.Monad.Reader (MonadReader, asks)
import           Data.Array.Bounded
import           Data.Bits (shiftL)
import           Data.Int
import           Data.Word
import           Linear.V2 (V2(..))

data SineData
  = SineData (BoundedArray Word16 Int16)
  deriving (Eq, Ord, Show)

class HasSineData a where
  sineData :: a -> SineData

instance HasSineData SineData where
  sineData =
    id

calcSine :: (HasSineData g, MonadReader g m) => Word8 -> m (Int16, Int16)
calcSine w =
  f <$> asks sineData
  where
    f (SineData a) =
      (a ! fromIntegral w, a ! fromIntegral (w + 0x40))

data AngleData
  = AngleData (BoundedArray Word8 Int8)
  deriving (Eq, Ord, Show)

class HasAngleData a where
  angleData :: a -> AngleData

instance HasAngleData AngleData where
  angleData =
    id

-- arctangent of y/x
calcAngle :: (HasAngleData g, MonadReader g m) => V2 Int16 -> m Int16
calcAngle (V2 x y) =
  h . g . f <$> asks angleData
  where
    x' =
      abs x
    y' =
      abs y
    f (AngleData angles) =
      if x == 0 && y == 0
      then 0x40
      else
        if y' < x'
        then fromIntegral (angles ! i (fromIntegral x') y')
        else 0x40 - fromIntegral (angles ! i (fromIntegral y') x')
    g =
      if x < 0
      then \r -> -r + 0x80
      else id
    h =
      if y < 0
      then \r -> -r + 0x100
      else id
    i a b =
      if a == 0
      then 0
      else fromIntegral (b `shiftL` 8) `div` a
