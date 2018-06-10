{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Player (
  Player(..)
, HasPosition(..)
, playerVelocity
, playerRadius
, playerTopSpeed
, playerAcceleration
, playerDeceleration
, playerInertia
, moveRight
, moveLeft
, settle
, traction
, normalTopSpeed
, normalAcceleration
, normalDeceleration
, underWaterTopSpeed
, underWaterAcceleration
, underWaterDeceleration
, pixels
, objectMove
) where

import           Control.Lens
import           Control.Monad.State
import           Data.Bits
import           Data.Halves           (finiteBitHalves)
import           Data.Int
import           Foreign.C.Types
import           Game.Sega.Sonic.Types
import           SDL

data Player
  = Player (V2 CInt) (V2 CInt) (V2 CInt) Int16 Int16 Int16 Int16
  deriving (Eq, Ord, Show)

instance HasPosition Player where
  position =
    lens y z
    where
      y (Player a _ _ _ _ _ _) =
        a
      z (Player _ b c d e f g) a =
        Player a b c d e f g

playerVelocity :: Lens' Player (V2 CInt)
playerVelocity =
  lens y z
  where
    y (Player _ b _ _ _ _ _) =
      b
    z (Player a _ c d e f g) b =
      Player a b c d e f g

playerRadius :: Lens' Player (V2 CInt)
playerRadius =
  lens y z
  where
    y (Player _ _ c _ _ _ _) =
      c
    z (Player a b _ d e f g) c =
      Player a b c d e f g

playerTopSpeed :: Lens' Player Int16
playerTopSpeed =
  lens y z
  where
    y (Player _ _ _ d _ _ _) =
      d
    z (Player a b c _ e f g) d =
      Player a b c d e f g

playerAcceleration :: Lens' Player Int16
playerAcceleration =
  lens y z
  where
    y (Player _ _ _ _ e _ _) =
      e
    z (Player a b c d _ f g) e =
      Player a b c d e f g

playerDeceleration :: Lens' Player Int16
playerDeceleration =
  lens y z
  where
    y (Player _ _ _ _ _ f _) =
      f
    z (Player a b c d e _ g) f =
      Player a b c d e f g

playerInertia :: Lens' Player Int16
playerInertia =
  lens y z
  where
    y (Player _ _ _ _ _ _ g) =
      g
    z (Player a b c d e f _) g =
      Player a b c d e f g

moveRight :: (MonadState Player m) => m ()
moveRight = do
  acceleration <- use playerAcceleration
  playerInertia += acceleration
  topSpeed <- use playerTopSpeed
  i <- use playerInertia
  unless (i < topSpeed) $ do
    playerInertia -= acceleration
    i' <- use playerInertia
    when (i' >= topSpeed) $
      playerInertia .= topSpeed

moveLeft :: (MonadState Player m) => m ()
moveLeft = do
  acceleration <- use playerAcceleration
  playerInertia -= acceleration
  topSpeed <- use playerTopSpeed
  i <- use playerInertia
  unless (i > -topSpeed) $ do
    playerInertia += acceleration
    i' <- use playerInertia
    when (i' <= -topSpeed) $
      playerInertia .= -topSpeed

settle :: (MonadState Player m) => m ()
settle = do
  i <- use playerInertia
  if i > 0
  then settleRight
  else settleLeft

settleRight :: (MonadState Player m) => m ()
settleRight = do
  playerInertia %= \i -> max 0 (i - 0xC)

settleLeft :: (MonadState Player m) => m ()
settleLeft = do
  playerInertia %= \i -> min 0 (i + 0xC)

traction :: (MonadState Player m) => m ()
traction = do
  -- let
  --   angle =
  --     0
  --   cosine =
  --     256
  --   sine =
  --     0
  inertia <- use playerInertia
  -- let
  --   x =
  --     (inertia * cosine) `shiftR` 8
  --   y =
  --     (inertia * sine) `shiftR` 8
  --   v =
  --     V2 (fromIntegral x) (fromIntegral y)
  -- playerVelocity .= v
  playerVelocity .= V2 (fromIntegral inertia) 0

cIntHalves :: Iso' CInt (Int16, Int16)
cIntHalves =
  finiteBitHalves

pixels :: Lens' (V2 CInt) (V2 Int16)
pixels =
  lens f g
  where
    f (V2 a b) =
      V2 (a ^. cIntHalves . _1)  (b ^. cIntHalves . _2)
    g (V2 a b) (V2 x y) =
      V2 (a & cIntHalves . _1 .~ x)  (b & cIntHalves . _2 .~ y)

objectMove :: (MonadState Player m) => m ()
objectMove = do
  velocity <- use playerVelocity
  position += ((`shiftL` 8) <$> velocity)

normalTopSpeed :: Int16
normalTopSpeed =
  0x600

normalAcceleration :: Int16
normalAcceleration =
  0xC

normalDeceleration :: Int16
normalDeceleration =
  0x80

underWaterTopSpeed :: Int16
underWaterTopSpeed =
  0x300

underWaterAcceleration :: Int16
underWaterAcceleration =
  0x6

underWaterDeceleration :: Int16
underWaterDeceleration =
  0x40
