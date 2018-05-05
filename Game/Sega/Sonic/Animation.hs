module Game.Sega.Sonic.Animation (
  AnimationScript(..)
, AnimationStep(..)
, loadAnimation
) where

import           Data.Array.Bounded
import           Data.Word          (Word8)

data AnimationScript
  = AnimationScript Word8 (BoundedArray Word8 AnimationStep)
  deriving (Eq, Ord, Show)

data AnimationStep
  = AnimationFrame Word8
  | AnimationReset
  | AnimationJumpBack Word8
  | AnimationNext Word8
  | AnimationIncrementRoutine
  | AnimationResetSubRoutine
  | AnimationIncrementSubRoutine
  deriving (Eq, Ord, Show)

loadAnimationSteps :: [Word8] -> [AnimationStep]
loadAnimationSteps (0xFF:bs) =
  AnimationReset : loadAnimationSteps bs
loadAnimationSteps (0xFE:b:bs) =
  AnimationJumpBack b : loadAnimationSteps bs
loadAnimationSteps (0xFD:b:bs) =
  AnimationNext b : loadAnimationSteps bs
loadAnimationSteps (0xFC:bs) =
  AnimationIncrementRoutine : loadAnimationSteps bs
loadAnimationSteps (0xFB:bs) =
  AnimationResetSubRoutine : loadAnimationSteps bs
loadAnimationSteps (0xFA:bs) =
  AnimationIncrementSubRoutine : loadAnimationSteps bs
loadAnimationSteps (b:bs) =
  AnimationFrame b : loadAnimationSteps bs
loadAnimationSteps [] =
  []

loadAnimation :: [Word8] -> AnimationScript
loadAnimation (b:bs) =
  AnimationScript b . listArrayFill AnimationReset $ loadAnimationSteps bs
