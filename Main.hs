{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Codec.Compression.Kosinski     as Kosinski
import           Control.Applicative            (liftA2)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Array.Bounded
import           Data.Array.Bounded             ((!))
import qualified Data.ByteString                as BS
import           Data.Foldable
import           Data.Semigroup                 ((<>))
import           Data.Time                      (diffUTCTime, getCurrentTime)
import           Game.Sega.Sonic.Animation
import           Game.Sega.Sonic.Blocks
import           Game.Sega.Sonic.Chunks
import           Game.Sega.Sonic.Collision
import           Game.Sega.Sonic.Error
import           Game.Sega.Sonic.Game
import           Game.Sega.Sonic.Layout
import           Game.Sega.Sonic.Offsets
import           Game.Sega.Sonic.Palette
import           Game.Sega.Sonic.SpriteMappings
import           Game.Sega.Sonic.Sprites
import           Game.Sega.Sonic.Tiles
import           SDL
import           Sega.MegaDrive.Palette

decompressFile :: (MonadReader BS.ByteString m, MonadError SonicError m, MonadIO m) => Offset -> m BS.ByteString
decompressFile offset = do
  maybeContent <- asks $ Kosinski.compressed . sliceOffset offset
  content <- maybe (throwError $ SonicLoadError offset) pure maybeContent
  maybe (throwError $ SonicDecompressionError offset) pure $ Kosinski.decompress content

renderLevelCollisions :: (MonadReader BS.ByteString m, MonadError SonicError m, MonadIO m) => Renderer -> LevelOffsets -> m [[Texture]]
renderLevelCollisions renderer offsets = do
  collisionIndexContent <- decompressFile $ levelCollisionOffset offsets
  collisionIndex <- loadCollisionIndex collisionIndexContent

  collisionContent <- asks $ sliceOffset collisionArray1
  collisionTextures <- liftIO $ loadCollisionTextures renderer collisionContent

  let reindexedCollisionTextures = (collisionTextures !) <$> collisionIndex

  now <- liftIO getCurrentTime
  chunksContent <- decompressFile $ levelChunksOffset offsets
  liftIO $ putStrLn "Loading chunks..."
  chunksTextures <- loadChunks renderer reindexedCollisionTextures chunksContent
  now' <- liftIO getCurrentTime
  liftIO . putStrLn $ "Chunks loaded in " <> show (diffUTCTime now' now)

  layoutContent <- decompressFile $ levelLayoutOffset offsets
  pure $ loadLayout chunksTextures layoutContent

renderLevelBlocks :: (MonadReader BS.ByteString m, MonadError SonicError m, MonadIO m) => Renderer -> LevelOffsets -> m [[Texture]]
renderLevelBlocks renderer offsets = do
  maybeSonicPalette <- asks $ readPalette . sliceOffset paletteSonic
  maybePalette <- asks $ readPalette . sliceOffset (levelPaletteOffset offsets)
  palette <- maybe (throwError . SonicPaletteError $ levelPaletteOffset offsets) (pure . loadPalette) (maybeSonicPalette <> maybePalette)
  tileContent <- decompressFile $ levelArtOffset offsets
  tileSurfaces <- loadTiles tileContent
  blockContent <- decompressFile $ levelBlocksOffset offsets
  blockTextures <- loadBlocks renderer palette tileSurfaces blockContent
  chunkContent <- decompressFile $ levelChunksOffset offsets
  chunkTextures <- loadChunks renderer blockTextures chunkContent
  layoutContent <- decompressFile $ levelLayoutOffset offsets
  pure $ loadLayout chunkTextures layoutContent

-- NTSC
frameRate :: Double
frameRate =
  29.97

main :: IO ()
main = do
  rom <- BS.readFile "sonic2.md"
  let

  window <- createWindow "Sonic 2" defaultWindow { windowInitialSize = V2 320 224 }
  renderer <- createRenderer window (-1) defaultRenderer
  rendererLogicalSize renderer $= Just (V2 320 224)

  Right chunkTextures <- runExceptT $ runReaderT (renderLevelBlocks renderer ehz1) rom
  -- Right collisionTextures <- runExceptT $ runReaderT (renderLevelCollisions renderer ehz1) rom

  let sonicContent = sliceOffset artSonic rom
      sonicMappings = loadMappings $ sliceOffset mappingSonic rom
      maybeSonicPalette = readPalette $ sliceOffset paletteSonic rom
      sonicDPLC = loadDynamicPatternLoadCues $ sliceOffset dplcSonic rom
  sonicSurfaces <- loadTiles sonicContent

  let tailsContent = sliceOffset artTails rom
      tailsMappings = loadMappings $ sliceOffset mappingTails rom
      tailsDPLC = loadDynamicPatternLoadCues $ sliceOffset dplcTails rom
  tailsSurfaces <- loadTiles tailsContent

  let
    Just palette =
      loadPalette <$> maybeSonicPalette
    f (SpriteFrame s) (DynamicPatternLoadCueFrame dplcs) = do
      sonicSurfaces' <- applyDynamicPatternLoadCue sonicSurfaces dplcs
      traverse (copySpriteMapping renderer palette sonicSurfaces') s
  sonicTextures <- traverse (uncurry f) $ zip sonicMappings sonicDPLC

  let
    g (SpriteFrame s) (DynamicPatternLoadCueFrame dplcs) = do
      tailsSurfaces' <- applyDynamicPatternLoadCue tailsSurfaces dplcs
      traverse (copySpriteMapping renderer palette tailsSurfaces') s
  tailsTextures <- traverse (uncurry g) $ zip tailsMappings tailsDPLC

  rendererRenderTarget renderer $= Nothing

  let
    sonicAnimationScript =
      loadAnimation . BS.unpack $ sliceOffset animationSonicWait rom
    tailsAnimationScript =
      loadAnimation . BS.unpack $ sliceOffset animationTailsWait rom
    sonic =
      Sprite sonicTextures (V2 100 660) sonicAnimationScript emptyAnimationState
    tails =
      Sprite tailsTextures (V2 50 660) tailsAnimationScript emptyAnimationState
    render textures o p =
      ifor_ textures $ \y row ->
        ifor_ row $ \x texture ->
          let
            rectangle =
              Rectangle (P (V2 ((fromIntegral x * 0x80) - o) ((fromIntegral y * 0x80) - p))) 0x80
          in copy renderer texture Nothing (Just rectangle)
    appLoop sonic' tails' game = do
      -- startTicks <- ticks
      events <- pollEvents
      let
        eventIsPress keycode event =
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
            _ ->
              False
        isPressed keycode =
          any (eventIsPress keycode) events
        qPressed =
          isPressed KeycodeQ
        rightPressed =
          isPressed KeycodeRight
        leftPressed =
          isPressed KeycodeLeft
        downPressed =
          isPressed KeycodeDown
        upPressed =
          isPressed KeycodeUp
        Game _ (V2 o p) =
          game
        o' =
          o + if rightPressed then 0x10 else 0 + if leftPressed then -0x10 else 0
        p' =
          p + if downPressed then 0x10 else 0 + if upPressed then -0x10 else 0
        game' =
          Game renderer (V2 o' p')
        sonic'' =
          stepSprite sonic'
        tails'' =
          stepSprite tails'
      rendererDrawColor renderer $= V4 0 0 0 0xFF
      clear renderer
      render chunkTextures o' p'
      runReaderT (renderSprite sonic' *> renderSprite tails') game'
      present renderer
      -- endTicks <- ticks
      -- let difference = fromIntegral endTicks - fromIntegral startTicks
      delay 31
      unless qPressed (appLoop sonic'' tails'' game')
  appLoop sonic tails (Game renderer 0)
