{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Player (
  Player(..)
, HasPlayer(..)
, Statuses(..)
, MdAir(..)
, MdRoll(..)
, HasPosition(..)
, PlayerRoutine(..)
, mdAir
, mdRoll
, playerRoutine
, playerVelocity
, playerRadius
, playerTopSpeed
, playerAcceleration
, playerDeceleration
, playerInertia
, playerAngle
, initialStatuses
, isJumping
, statuses
, resetOnFloor
, jump
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
, objectMoveAndFall
) where

import           Control.Lens
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State
import           Data.Bits
import           Data.Halves           (finiteBitHalves)
import           Data.Int
import           Data.Word             (Word8)
import           Foreign.C.Types
import           Game.Sega.Sonic.Sine
import           Game.Sega.Sonic.Types
import           SDL

import           Debug.Trace

data Player
  = Player (V2 CInt) (V2 Int16) (V2 CInt) Int16 Int16 Int16 Int16 Word8 Statuses
  deriving (Eq, Ord, Show)

class HasPlayer a where
  player :: Lens' a Player

instance HasPlayer Player where
  player =
    id

data Statuses
  = Statuses MdAir MdRoll
  deriving (Eq, Ord, Show)

initialStatuses :: Statuses
initialStatuses =
  Statuses MdAirOff MdRollOff

mdAir :: Lens' Statuses MdAir
mdAir =
  lens f g
  where
    f (Statuses a _) =
      a
    g (Statuses _ b) a =
      Statuses a b

mdRoll :: Lens' Statuses MdRoll
mdRoll =
  lens f g
  where
    f (Statuses _ b) =
      b
    g (Statuses a _) b =
      Statuses a b

isJumping :: Statuses -> Bool
isJumping s =
  s ^. mdAir == MdAirOn && s ^. mdRoll == MdRollOn

data PlayerRoutine
  = MdNormal
  | MdAir
  | MdRoll
  | MdJump
  deriving Show

playerRoutine :: Statuses -> PlayerRoutine
playerRoutine (Statuses MdAirOff MdRollOff) =
  MdNormal
playerRoutine (Statuses MdAirOff MdRollOn) =
  MdRoll
playerRoutine (Statuses MdAirOn MdRollOff) =
  MdAir
playerRoutine (Statuses MdAirOn MdRollOn) =
  MdJump

data MdAir
  = MdAirOn
  | MdAirOff
  deriving (Eq, Ord, Show)

data MdRoll
  = MdRollOn
  | MdRollOff
  deriving (Eq, Ord, Show)

instance HasPosition Player where
  position =
    lens y z
    where
      y (Player a _ _ _ _ _ _ _ _) =
        a
      z (Player _ b c d e f g h i) a =
        Player a b c d e f g h i

playerVelocity :: Lens' Player (V2 Int16)
playerVelocity =
  lens y z
  where
    y (Player _ b _ _ _ _ _ _ _) =
      b
    z (Player a _ c d e f g h i) b =
      Player a b c d e f g h i

playerRadius :: Lens' Player (V2 CInt)
playerRadius =
  lens y z
  where
    y (Player _ _ c _ _ _ _ _ _) =
      c
    z (Player a b _ d e f g h i) c =
      Player a b c d e f g h i

playerTopSpeed :: Lens' Player Int16
playerTopSpeed =
  lens y z
  where
    y (Player _ _ _ d _ _ _ _ _) =
      d
    z (Player a b c _ e f g h i) d =
      Player a b c d e f g h i

playerAcceleration :: Lens' Player Int16
playerAcceleration =
  lens y z
  where
    y (Player _ _ _ _ e _ _ _ _) =
      e
    z (Player a b c d _ f g h i) e =
      Player a b c d e f g h i

playerDeceleration :: Lens' Player Int16
playerDeceleration =
  lens y z
  where
    y (Player _ _ _ _ _ f _ _ _) =
      f
    z (Player a b c d e _ g h i) f =
      Player a b c d e f g h i

playerInertia :: Lens' Player Int16
playerInertia =
  lens y z
  where
    y (Player _ _ _ _ _ _ g _ _) =
      g
    z (Player a b c d e f _ h i) g =
      Player a b c d e f g h i

playerAngle :: Lens' Player Word8
playerAngle =
  lens y z
  where
    y (Player _ _ _ _ _ _ _ h _) =
      h
    z (Player a b c d e f g _ i) h =
      Player a b c d e f g h i

statuses :: Lens' Player Statuses
statuses =
  lens y z
  where
    y (Player _ _ _ _ _ _ _ _ i) =
      i
    z (Player a b c d e f g h _) i =
      Player a b c d e f g h i

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

traction :: (HasPlayer s, MonadState s m) => m ()
traction = do
  -- let
  --   angle =
  --     0
  --   cosine =
  --     256
  --   sine =
  --     0
  inertia <- use (player . playerInertia)
  -- let
  --   x =
  --     (inertia * cosine) `shiftR` 8
  --   y =
  --     (inertia * sine) `shiftR` 8
  --   v =
  --     V2 (fromIntegral x) (fromIntegral y)
  -- playerVelocity .= v
  player . playerVelocity .= V2 (fromIntegral inertia) 0

cIntHalves :: Iso' CInt (Int16, Int16)
cIntHalves =
  finiteBitHalves

pixels :: Lens' (V2 CInt) (V2 Int16)
pixels =
  lens f g
  where
    f (V2 a b) =
      V2 (a ^. cIntHalves . _1)  (b ^. cIntHalves . _1)
    g (V2 a b) (V2 x y) =
      V2 (a & cIntHalves . _1 .~ x)  (b & cIntHalves . _1 .~ y)

jump :: (HasSineData a, MonadReader a m, MonadState Player m) => m ()
jump = do
  angle' <- use playerAngle
  (sine, cosine) <- calcSine (angle' - 0x40)
  let
    jumpSpeed :: Int32
    jumpSpeed =
      0x680
    x :: Int16
    x =
      fromIntegral $ (jumpSpeed * fromIntegral cosine) `shiftR` 8
    y :: Int16
    y =
      fromIntegral $ (jumpSpeed * fromIntegral sine) `shiftR` 8
  statuses . mdAir .= MdAirOn
  statuses . mdRoll .= MdRollOn
  playerVelocity . _x += x
  playerVelocity . _y += y

objectMoveAndFall :: (MonadState Player m) => m ()
objectMoveAndFall = do
  velocity <- use playerVelocity
  playerVelocity . _y += 0x38
  position += ((`shiftL` 8) . fromIntegral <$> velocity)

objectMove :: (MonadState Player m) => m ()
objectMove = do
  velocity <- use playerVelocity
  position += ((`shiftL` 8) . fromIntegral <$> velocity)

resetOnFloor :: (HasPlayer s, MonadState s m) => m ()
resetOnFloor = do
  player . statuses . mdAir .= MdAirOff
  player . statuses . mdRoll .= MdRollOff

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
