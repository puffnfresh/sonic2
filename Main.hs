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
import           Data.ByteString.Lens
import           Data.Foldable
import           Data.Halves                    (collectHalves)
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
import           Game.Sega.Sonic.Player
import           Game.Sega.Sonic.SpriteMappings
import           Game.Sega.Sonic.Sprites
import           Game.Sega.Sonic.Tiles
import           SDL
import           Sega.MegaDrive.Palette

decompressFile :: (HasRom g, MonadReader g m, MonadError SonicError m, MonadIO m) => Offset -> m BS.ByteString
decompressFile offset = do
  maybeContent <- Kosinski.compressed <$> sliceRom offset
  content <- maybe (throwError $ SonicLoadError offset) pure maybeContent
  maybe (throwError $ SonicDecompressionError offset) pure $ Kosinski.decompress content

renderLevelCollisions :: (HasRom g, HasRenderer g, MonadReader g m, MonadError SonicError m, MonadIO m) => LevelOffsets -> m [[Texture]]
renderLevelCollisions offsets = do
  collisionIndexContent <- decompressFile $ levelCollisionOffset offsets
  collisionIndex <- loadCollisionIndex collisionIndexContent

  collisionContent <- sliceRom collisionArray1
  collisionTextures <- loadCollisionTextures collisionContent

  let reindexedCollisionTextures = (collisionTextures !) <$> collisionIndex

  now <- liftIO getCurrentTime
  chunksContent <- decompressFile $ levelChunksOffset offsets
  liftIO $ putStrLn "Loading chunks..."
  let chunksBlocks = loadChunks chunksContent
  chunksTextures <- traverse (loadChunkTexture reindexedCollisionTextures) chunksBlocks
  now' <- liftIO getCurrentTime
  liftIO . putStrLn $ "Chunks loaded in " <> show (diffUTCTime now' now)

  layoutContent <- decompressFile $ levelLayoutOffset offsets
  let
    layout =
      loadLayout layoutContent
  pure $ mapChunkTextures chunksTextures layout

-- NTSC
frameRate :: Double
frameRate =
  60

loadAndRun :: (MonadReader Game m, MonadError SonicError m, MonadIO m) => m ()
loadAndRun = do


  sonicMappings <- loadSpriteMappings sonicOffsets
  tailsMappings <- loadSpriteMappings tailsOffsets

  sonicAnimationScript <- loadAnimation . BS.unpack <$> sliceRom animationSonicWait
  tailsAnimationScript <- loadAnimation . BS.unpack <$> sliceRom animationTailsWait

  let offsets = ehz1
  maybeSonicPalette <- readPalette <$> sliceRom paletteSonic
  maybePalette <- readPalette <$> sliceRom (levelPaletteOffset offsets)
  palette <- maybe (throwError . SonicPaletteError $ levelPaletteOffset offsets) (pure . loadPalette) (maybeSonicPalette <> maybePalette)
  tileContent <- decompressFile $ levelArtOffset offsets
  tileSurfaces <- loadTiles tileContent
  blockContent <- decompressFile $ levelBlocksOffset offsets
  blockTextures <- loadBlocks palette tileSurfaces blockContent
  chunkContent <- decompressFile $ levelChunksOffset offsets
  let chunkBlocks = loadChunks chunkContent
  chunkTextures <- traverse (loadChunkTexture blockTextures) chunkBlocks
  layoutContent <- decompressFile $ levelLayoutOffset offsets
  let
    layout =
      loadLayout layoutContent
    layoutChunkTextures =
      mapChunkTextures chunkTextures layout

  -- chunkTextures <- renderLevelBlocks ehz1
  collisionTextures <- renderLevelCollisions ehz1
  startPos <- sliceRom $ levelStartPos ehz1
  let
    playerStart =
      case (startPos ^. unpackedBytes . collectHalves) of
        [x, y] ->
          V2 (fromIntegral x) (fromIntegral y)
        _ ->
          V2 0 0

  r <- view renderer
  rendererRenderTarget r $= Nothing

  let
    playerSprite =
      Sprite sonicMappings (V2 0 0) sonicAnimationScript emptyAnimationState
    render textures o p =
      ifor_ textures $ \y row ->
        ifor_ row $ \x texture ->
          let
            rectangle =
              Rectangle (P (V2 ((fromIntegral x * 0x80) - o) ((fromIntegral y * 0x80) - p))) 0x80
          in copy r texture Nothing (Just rectangle)
    appLoop playerSprite' game = do
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
        o =
          game ^. camera . cameraX
        p =
          game ^. camera . cameraY
        o' =
          o + if rightPressed then 0x10 else 0 + if leftPressed then -0x10 else 0
        p' =
          p + if downPressed then 0x10 else 0 + if upPressed then -0x10 else 0
        playerSprite'' =
          playerSprite' & position .~ (game ^. player . position)
        game' =
          game & camera .~ V2 o' p'
      rendererDrawColor r $= V4 0 0 0 0xFF
      clear r
      render layoutChunkTextures o' p'
      render collisionTextures o' p'
      runReaderT (renderSprite playerSprite'') game'
      let
        V2 layoutX layoutY =
          (`div` 0x80) <$> (game ^. player . position) + V2 0 0x13
        chunkIndex =
          layout !! fromIntegral layoutY !! fromIntegral layoutX
      -- liftIO $ print chunkIndex
      copy r (chunkTextures ! chunkIndex) Nothing (Just $ Rectangle (P (V2 0 0)) (V2 128 128))
      present r
      -- endTicks <- ticks
      -- let difference = fromIntegral endTicks - fromIntegral startTicks
      delay 16
      unless qPressed (appLoop (stepSprite playerSprite'') game')
  game <- ask
  appLoop playerSprite (game & player . position .~ playerStart)

main :: IO ()
main = do
  rom' <- BS.readFile "sonic2.md"
  let

  window <- createWindow "Sonic 2" defaultWindow { windowInitialSize = V2 320 224 }
  renderer' <- createRenderer window (-1) defaultRenderer
  rendererLogicalSize renderer' $= Just (V2 320 224)

  e <- runReaderT (runExceptT loadAndRun) (Game renderer' 0 rom' $ Player (V2 0 0) (V2 0 0x13))
  either print pure e
