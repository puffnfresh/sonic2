{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Codec.Compression.Kosinski as Kosinski
import           Control.Applicative        (liftA2)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Array.Bounded         ((!))
import qualified Data.ByteString            as BS
import           Data.Semigroup             ((<>))
import           Data.Time                  (diffUTCTime, getCurrentTime)
import           Game.Sega.Sonic.Blocks
import           Game.Sega.Sonic.Cells
import           Game.Sega.Sonic.Chunks
import           Game.Sega.Sonic.Collision
import           Game.Sega.Sonic.Error
import           Game.Sega.Sonic.Layout
import           Game.Sega.Sonic.Palette
import           SDL
import           Sega.MegaDrive.Palette
import           System.FilePath.Posix

data LevelPaths
  = LevelPaths { levelLayoutPath    :: FilePath
               , levelChunksPath    :: FilePath
               , levelBlocksPath    :: FilePath
               , levelCollisionPath :: FilePath
               , levelPalettePath   :: FilePath
               , levelArtPath       :: FilePath
               }

ehz1Paths :: LevelPaths
ehz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "EHZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "EHZ_HTZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "EHZ.bin")
             ("s2disasm" </> "collision" </> "EHZ and HTZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "EHZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "EHZ_HTZ.bin")

ehz2Paths :: LevelPaths
ehz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "EHZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "EHZ_HTZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "EHZ.bin")
             ("s2disasm" </> "collision" </> "EHZ and HTZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "EHZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "EHZ_HTZ.bin")

cpz1Paths :: LevelPaths
cpz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "CPZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "collision" </> "CPZ and DEZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "CPZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "CPZ_DEZ.bin")

cpz2Paths :: LevelPaths
cpz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "CPZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "collision" </> "CPZ and DEZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "CPZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "CPZ_DEZ.bin")

arz1Paths :: LevelPaths
arz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "ARZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "ARZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "ARZ.bin")
             ("s2disasm" </> "collision" </> "ARZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "ARZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "ARZ.bin")


arz2Paths :: LevelPaths
arz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "ARZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "ARZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "ARZ.bin")
             ("s2disasm" </> "collision" </> "ARZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "ARZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "ARZ.bin")

cnz1Paths :: LevelPaths
cnz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "CNZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "CNZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "CNZ.bin")
             ("s2disasm" </> "collision" </> "CNZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "CNZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "CNZ.bin")

cnz2Paths :: LevelPaths
cnz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "CNZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "CNZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "CNZ.bin")
             ("s2disasm" </> "collision" </> "CNZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "CNZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "CNZ.bin")

htz1Paths :: LevelPaths
htz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "HTZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "EHZ_HTZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "HTZ.bin")
             ("s2disasm" </> "collision" </> "EHZ and HTZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "HTZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "EHZ_HTZ.bin")

htz2Paths :: LevelPaths
htz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "HTZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "EHZ_HTZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "HTZ.bin")
             ("s2disasm" </> "collision" </> "EHZ and HTZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "HTZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "EHZ_HTZ.bin")

mcz1Paths :: LevelPaths
mcz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "MCZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "MCZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "MCZ.bin")
             ("s2disasm" </> "collision" </> "MCZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "MCZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "MCZ.bin")

mcz2Paths :: LevelPaths
mcz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "MCZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "MCZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "MCZ.bin")
             ("s2disasm" </> "collision" </> "MCZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "MCZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "MCZ.bin")

ooz1Paths :: LevelPaths
ooz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "OOZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "OOZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "OOZ.bin")
             ("s2disasm" </> "collision" </> "OOZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "OOZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "OOZ.bin")

ooz2Paths :: LevelPaths
ooz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "OOZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "OOZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "OOZ.bin")
             ("s2disasm" </> "collision" </> "OOZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "OOZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "OOZ.bin")

mtz1Paths :: LevelPaths
mtz1Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "MTZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "MTZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "MTZ.bin")
             ("s2disasm" </> "collision" </> "MTZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "MTZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "MTZ.bin")

mtz2Paths :: LevelPaths
mtz2Paths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "MTZ_2.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "MTZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "MTZ.bin")
             ("s2disasm" </> "collision" </> "MTZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "MTZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "MTZ.bin")

sczPaths :: LevelPaths
sczPaths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "SCZ.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "WFZ_SCZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "WFZ_SCZ.bin")
             ("s2disasm" </> "collision" </> "WFZ and SCZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "SCZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "WFZ_SCZ.bin")

wfzPaths :: LevelPaths
wfzPaths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "WFZ.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "WFZ_SCZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "WFZ_SCZ.bin")
             ("s2disasm" </> "collision" </> "WFZ and SCZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "WFZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "WFZ_SCZ.bin")

dezPaths :: LevelPaths
dezPaths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "DEZ.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "collision" </> "CPZ and DEZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "DEZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "CPZ_DEZ.bin")

decompressFile :: (MonadError SonicError m, MonadIO m) => FilePath -> m BS.ByteString
decompressFile path = do
  maybeContent <- liftIO $ Kosinski.compressedFile path
  content <- maybe (throwError $ SonicLoadError path) pure maybeContent
  maybe (throwError $ SonicDecompressionError path) pure $ Kosinski.decompress content

renderLevelCollisions :: (MonadError SonicError m, MonadIO m) => Renderer -> LevelPaths -> m [[Texture]]
renderLevelCollisions renderer paths = do
  collisionIndexContent <- decompressFile $ levelCollisionPath paths
  collisionIndex <- loadCollisionIndex collisionIndexContent

  collisionContent <- liftIO $ BS.readFile ("s2disasm" </> "collision" </> "Collision array 1.bin")
  collisionTextures <- liftIO $ loadCollisionTextures renderer collisionContent

  let reindexedCollisionTextures = (collisionTextures !) <$> collisionIndex

  now <- liftIO getCurrentTime
  chunksContent <- decompressFile $ levelChunksPath paths
  liftIO $ putStrLn "Loading chunks..."
  chunksTextures <- loadChunks renderer reindexedCollisionTextures chunksContent
  now' <- liftIO getCurrentTime
  liftIO . putStrLn $ "Chunks loaded in " <> show (diffUTCTime now' now)

  layoutContent <- decompressFile $ levelLayoutPath paths
  pure $ loadLayout chunksTextures layoutContent

renderLevelBlocks :: (MonadError SonicError m, MonadIO m) => Renderer -> LevelPaths -> m [[Texture]]
renderLevelBlocks renderer paths = do
  maybePalette <- liftIO $ readPalette <$> BS.readFile (levelPalettePath paths)
  palette <- maybe (throwError . SonicPaletteError $ levelPalettePath paths) (pure . loadPalette) maybePalette
  cellContent <- decompressFile $ levelArtPath paths
  cellSurfaces <- loadCells cellContent
  blockContent <- decompressFile $ levelBlocksPath paths
  blockTextures <- loadBlocks renderer palette cellSurfaces blockContent
  chunkContent <- decompressFile $ levelChunksPath paths
  chunkTextures <- loadChunks renderer blockTextures chunkContent
  layoutContent <- decompressFile $ levelLayoutPath paths
  pure $ loadLayout chunkTextures layoutContent

main :: IO ()
main = do
  window <- createWindow "Sonic 2" defaultWindow { windowInitialSize = V2 960 672 }
  renderer <- createRenderer window (-1) defaultRenderer
  rendererLogicalSize renderer $= Just (V2 320 224)

  Right (chunkTextures, collisionTextures) <- runExceptT $ runReaderT (liftA2 (,) (ReaderT $ renderLevelBlocks renderer) (ReaderT $ renderLevelCollisions renderer)) ehz1Paths

  rendererRenderTarget renderer $= Nothing

  let
    render textures o p =
      ifor_ textures $ \y row ->
        ifor_ row $ \x texture ->
          let
            rectangle =
              Rectangle (P (V2 ((fromIntegral x * 0x80) - o) ((fromIntegral y * 0x80) - p))) 0x80
          in copy renderer texture Nothing (Just rectangle)
    appLoop o p c = do
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
      rendererDrawColor renderer $= V4 0 0 0 0xFF
      clear renderer
      render chunkTextures o' p'
      when c $ render collisionTextures o' p'
      present renderer
      unless qPressed (appLoop o' p' c')
  appLoop 0 0 False
