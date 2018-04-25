{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Codec.Compression.Kosinski as Kosinski
import           Control.Lens
import           Control.Monad.Except
import           Data.Array
import qualified Data.ByteString            as BS
import           Data.Semigroup             ((<>))
import           Data.Time                  (diffUTCTime, getCurrentTime)
import           Data.Word
import           Game.Sega.Sonic.Chunks
import           Game.Sega.Sonic.Collision
import           Game.Sega.Sonic.Layout
import           SDL
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

dezPaths :: LevelPaths
dezPaths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "DEZ.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "CPZ_DEZ.bin")
             ("s2disasm" </> "collision" </> "CPZ and DEZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "DEZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "CPZ_DEZ.bin")

data SonicError
  = SonicLoadError FilePath
  | SonicDecompressionError FilePath
  deriving (Eq, Ord, Show)

decompressFile :: (MonadError SonicError m, MonadIO m) => FilePath -> m [Word8]
decompressFile path = do
  maybeContent <- liftIO $ Kosinski.compressedFile path
  content <- maybe (throwError $ SonicLoadError path) pure maybeContent
  maybe (throwError $ SonicDecompressionError path) pure $ Kosinski.decompress content

renderLevelCollisions :: (MonadError SonicError m, MonadIO m) => Renderer -> LevelPaths -> m [[Texture]]
renderLevelCollisions renderer paths = do
  collisionIndexContent <- decompressFile $ levelCollisionPath paths
  let collisionIndex = loadCollisionIndex collisionIndexContent

  collisionContent <- liftIO $ BS.readFile ("s2disasm" </> "collision" </> "Collision array 1.bin")
  collisionTextures <- liftIO $ loadCollisionTextures renderer collisionContent

  let reindexedCollsionTextures = (collisionTextures !) <$> collisionIndex

  now <- liftIO getCurrentTime
  chunksContent <- decompressFile $ levelChunksPath paths
  liftIO $ putStrLn "Loading chunks..."
  chunksTextures <- loadChunks renderer reindexedCollsionTextures chunksContent
  now' <- liftIO getCurrentTime
  liftIO . putStrLn $ "Chunks loaded in " <> show (diffUTCTime now' now)

  layoutContent <- decompressFile $ levelLayoutPath paths
  pure $ loadLayout chunksTextures layoutContent

main :: IO ()
main = do
  window <- createWindow "Sonic 2" defaultWindow { windowInitialSize = V2 1024 768 }
  renderer <- createRenderer window (-1) defaultRenderer

  Right chunkTextures <- runExceptT $ renderLevelCollisions renderer cpz1Paths

  rendererRenderTarget renderer $= Nothing

  let
    appLoop o p = do
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
        o' =
          o + if rightPressed then 0x10 else 0 + if leftPressed then -0x10 else 0
        p' =
          p + if downPressed then 0x10 else 0 + if upPressed then -0x10 else 0
      rendererDrawColor renderer $= V4 0 0 255 255
      clear renderer
      ifor_ chunkTextures $ \y row ->
        ifor_ row $ \x texture ->
          let
            rectangle =
              Rectangle (P (V2 ((fromIntegral x * 0x80) - o') ((fromIntegral y * 0x80) - p))) 0x80
          in copy renderer texture Nothing (Just rectangle)
      present renderer
      unless qPressed (appLoop o' p')
  appLoop 0 0
