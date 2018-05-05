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

limitFrameRate :: Double -> IO a -> IO a
limitFrameRate frameRate action = do
  startTicks <- ticks
  a <- action
  endTicks <- ticks
  let difference = fromIntegral endTicks - fromIntegral startTicks
  delay (floor $ ((1000.0 / frameRate) - difference))
  pure a

main :: IO ()
main = do
  rom <- BS.readFile "sonic2.md"
  let
    AnimationScript _ animationSteps =
      loadAnimation . BS.unpack $ sliceOffset animationSonicWait rom
    animationSteps' =
      listArrayFill AnimationReset $ animationSteps
    -- NTSC vs PAL
    frameRate =
      29.97

  window <- createWindow "Sonic 2" defaultWindow { windowInitialSize = V2 320 224 }
  renderer <- createRenderer window (-1) defaultRenderer
  rendererLogicalSize renderer $= Just (V2 320 224)

  Right chunkTextures <- runExceptT $ runReaderT (renderLevelBlocks renderer ehz1) rom
  Right collisionTextures <- runExceptT $ runReaderT (renderLevelCollisions renderer ehz1) rom

  let sonicContent = sliceOffset artSonic rom
      sonicMappings = loadMappings $ sliceOffset mappingSonic rom
      maybeSonicPalette = readPalette $ sliceOffset paletteSonic rom
      sonicDPLC = loadDynamicPatternLoadCues $ sliceOffset dplcSonic rom
  sonicSurfaces <- loadTiles sonicContent

  let
    Just palette =
      loadPalette <$> maybeSonicPalette
    f (SpriteFrame s) (DynamicPatternLoadCueFrame dplcs) = do
      sonicSurfaces' <- applyDynamicPatternLoadCue sonicSurfaces dplcs
      traverse (copySpriteMapping renderer palette sonicSurfaces') s
  sonicTextures <- traverse (uncurry f) $ zip sonicMappings sonicDPLC

  rendererRenderTarget renderer $= Nothing

  let
    render textures o p =
      ifor_ textures $ \y row ->
        ifor_ row $ \x texture ->
          let
            rectangle =
              Rectangle (P (V2 ((fromIntegral x * 0x80) - o) ((fromIntegral y * 0x80) - p))) 0x80
          in copy renderer texture Nothing (Just rectangle)
    appLoop o p (n, m) c = limitFrameRate frameRate $ do
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
        cPressed =
          isPressed KeycodeC
        o' =
          o + if rightPressed then 0x10 else 0 + if leftPressed then -0x10 else 0
        p' =
          p + if downPressed then 0x10 else 0 + if upPressed then -0x10 else 0
        c' =
          (if cPressed then not else id) c
        (n', m') =
          case animationSteps' ! n of
            AnimationFrame m''  -> (n + 1, m'')
            AnimationJumpBack j -> (n - j, m)
            _                   -> (n + 1, m)
        renderSprite x ts =
          for_ ts $ \(SpriteMapping l t w h e) ->
            copy renderer e Nothing (Just $ Rectangle (P (V2 (fromIntegral l + x) ((fromIntegral t + 655) - p'))) (V2 (fromIntegral w) (fromIntegral h)))
      rendererDrawColor renderer $= V4 0 0 0 0xFF
      clear renderer
      render chunkTextures o' p'
      when c $ render collisionTextures o' p'
      renderSprite (100 - o') (sonicTextures !! fromIntegral m')
      present renderer
      unless qPressed (appLoop o' p' (n', m') c')
  appLoop 0 0 (0, 0) False
