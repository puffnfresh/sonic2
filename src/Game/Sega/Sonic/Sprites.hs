{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Sprites (
  copySpriteTile
, copySpriteMapping
, AnimationState(..)
, emptyAnimationState
, stepAnimation
, Sprite(..)
, stepSprite
, spriteAnimationState
, renderSprite
, loadSpriteMappings
) where

import           Control.Applicative            (liftA2)
import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader)
import           Data.Array.Bounded
import           Data.Bits                      (shiftR, testBit, (.&.))
import           Data.Foldable                  (for_)
import           Data.Vector.Storable           (Vector)
import           Data.Word                      (Word16, Word8)
import           Foreign.C.Types                (CInt)
import           Game.Sega.Sonic.Animation      (AnimationScript (..),
                                                 AnimationStep (..))
import           Game.Sega.Sonic.Game
import           Game.Sega.Sonic.Offsets
import           Game.Sega.Sonic.Palette        (loadPalette)
import           Game.Sega.Sonic.SpriteMappings
import           Game.Sega.Sonic.Tiles          (applyDynamicPatternLoadCue,
                                                 loadTiles)
import           Game.Sega.Sonic.Types          (HasPosition (..))
import           SDL                            hiding (Vector)
import           Sega.MegaDrive.Palette         (readPalette)

data AnimationState
  = AnimationState Word8 Word8 Word8
  deriving (Eq, Ord, Show)

emptyAnimationState :: AnimationState
emptyAnimationState =
  AnimationState 0 0 0

stepAnimation :: AnimationScript -> AnimationState -> AnimationState
stepAnimation (AnimationScript spriteDelay steps) (AnimationState stepIndex spriteIndex spriteCount) =
  if spriteCount >= spriteDelay
  then state'
  else AnimationState stepIndex spriteIndex (spriteCount + 1)
  where
    state' =
      case steps ! stepIndex of
        AnimationFrame spriteIndex'  -> AnimationState (stepIndex + 1) spriteIndex' 0
        AnimationJumpBack j -> AnimationState (stepIndex - j) spriteIndex 0
        _                   -> AnimationState (stepIndex + 1) spriteIndex 0

data Sprite
  = Sprite [[SpriteMapping Texture]] (V2 CInt) AnimationScript AnimationState

instance HasPosition Sprite where
  position =
    lens f g
    where
      f (Sprite _ a _ _) =
        a
      g (Sprite a _ c d) b =
        Sprite a b c d

stepSprite :: Sprite -> Sprite
stepSprite sprite =
  sprite & spriteAnimationState %~ stepAnimation (sprite ^. spriteAnimationScript)

spriteAnimationScript :: Lens' Sprite AnimationScript
spriteAnimationScript =
  lens f g
  where
    f (Sprite _ _ a _) =
      a
    g (Sprite a b _ d) c =
      Sprite a b c d

spriteAnimationState :: Lens' Sprite AnimationState
spriteAnimationState =
  lens f g
  where
    f (Sprite _ _ _ a) =
      a
    g (Sprite a b c _) d =
      Sprite a b c d

renderSprite :: (HasRenderer g, HasCamera g, MonadReader g m, MonadIO m) => Sprite -> m ()
renderSprite (Sprite mappings (V2 x y) _ (AnimationState _ m _)) = do
  r <- view renderer
  o <- view (camera . cameraX)
  p <- view (camera . cameraY)
  for_ (mappings !! fromIntegral m) $ \(SpriteMapping l t w h e) ->
    copy r e Nothing . Just $ Rectangle (P (V2 (fromIntegral l + x - o) (fromIntegral t + y - p))) (V2 (fromIntegral w) (fromIntegral h))

copySpriteTile :: (HasRenderer g, MonadReader g m, MonadIO m) => BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> Word16 -> V2 CInt -> m ()
copySpriteTile palette sprite c v = do
  let
    tileSurface =
      sprite ! (c .&. 0x7FF)
    paletteIndex =
      fromIntegral (c `shiftR` 13 .&. 0x3)
    flipY =
      testBit c 0xC
    flipX =
      testBit c 0xB
  format <- surfaceFormat tileSurface
  maybeTilePalette <- formatPalette format
  for_ maybeTilePalette $ \tilePalette ->
    setPaletteColors tilePalette (palette ! paletteIndex) 0
  r <- view renderer
  tileTexture <- createTextureFromSurface r tileSurface
  copyEx r tileTexture Nothing (Just $ Rectangle (P v) 8) 0 Nothing $ V2 flipX flipY
  destroyTexture tileTexture

copySpriteMapping :: (HasRenderer g, MonadReader g m, MonadIO m) => BoundedArray Word8 (Vector (V4 Word8)) -> BoundedArray Word16 Surface -> SpriteMapping PatternIndex -> m (SpriteMapping Texture)
copySpriteMapping palette tiles (SpriteMapping top left width height (PatternIndex patternIndex)) = do
  let
    width' =
      8 * (width + 1)
    height' =
      8 * (height + 1)
  r <- view renderer
  texture <- createTexture r ABGR8888 TextureAccessTarget $ V2 (fromIntegral width') (fromIntegral height')
  rendererRenderTarget r $= Just texture
  textureBlendMode texture $= BlendAlphaBlend
  ifor_ (liftA2 (,) [0..width] [0..height]) $ \i (x, y) ->
    let
      patternIndex' =
        patternIndex + fromIntegral i
      position =
        V2 (fromIntegral x * 8) (fromIntegral y * 8)
    in copySpriteTile palette tiles patternIndex' position
  pure $ SpriteMapping top left width' height' texture

loadSpriteMappings :: (HasRom g, HasRenderer g, MonadReader g m, MonadIO m) => SpriteOffsets -> m [[SpriteMapping Texture]]
loadSpriteMappings offsets = do
  content <- sliceRom $ spriteArt offsets
  mappings <- loadMappings <$> sliceRom (spriteMapping offsets)
  maybePalette <- readPalette <$> sliceRom (spritePalette offsets)
  dplc <- loadDynamicPatternLoadCues <$> sliceRom (spriteDPLC offsets)
  surfaces <- loadTiles content
  let
    Just palette =
      loadPalette <$> maybePalette
    f (SpriteFrame s) (DynamicPatternLoadCueFrame dplcs) = do
      sonicSurfaces' <- applyDynamicPatternLoadCue surfaces dplcs
      traverse (copySpriteMapping palette sonicSurfaces') s
  traverse (uncurry f) $ zip mappings dplc
