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

ehzPaths :: LevelPaths
ehzPaths =
  LevelPaths ("s2disasm" </> "level" </> "layout" </> "EHZ_1.bin")
             ("s2disasm" </> "mappings" </> "128x128" </> "EHZ_HTZ.bin")
             ("s2disasm" </> "mappings" </> "16x16" </> "EHZ.bin")
             ("s2disasm" </> "collision" </> "EHZ and HTZ primary 16x16 collision index.bin")
             ("s2disasm" </> "art" </> "palettes" </> "EHZ.bin")
             ("s2disasm" </> "art" </> "kosinski" </> "EHZ_HTZ.bin")

data SonicError
  = SonicLoadError FilePath
  | SonicDecompressionError FilePath
  deriving (Eq, Ord, Show)

decompressFile :: (MonadError SonicError m, MonadIO m) => FilePath -> m [Word8]
decompressFile path = do
  maybeContent <- liftIO $ Kosinski.compressedFile path
  content <- maybe (throwError $ SonicLoadError path) pure maybeContent
  maybe (throwError $ SonicDecompressionError path) pure $ Kosinski.decompress content

renderLevelCollisions :: (MonadError SonicError m, MonadIO m) => Renderer -> LevelPaths -> m (Array Word8 Texture)
renderLevelCollisions renderer paths = do
  collisionIndexContent <- decompressFile $ levelCollisionPath paths
  let collisionIndex = loadCollisionIndex collisionIndexContent

  collisionContent <- liftIO $ BS.readFile ("s2disasm" </> "collision" </> "Collision array 1.bin")
  collisionTextures <- liftIO $ loadCollisionTextures renderer collisionContent

  let reindexedCollsionTextures = (collisionTextures !) <$> collisionIndex

  liftIO $ putStrLn "Loading chunks..."
  now <- liftIO getCurrentTime
  chunksContent <- decompressFile $ levelChunksPath paths
  chunksTextures <- loadChunks renderer reindexedCollsionTextures chunksContent
  now' <- liftIO getCurrentTime
  liftIO . putStrLn $ "Chunks loaded in " <> show (diffUTCTime now' now)
  pure chunksTextures

main :: IO ()
main = do
  window <- createWindow "Sonic 2" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  Right chunkTextures <- runExceptT $ renderLevelCollisions renderer ehzPaths

  rendererRenderTarget renderer $= Nothing

  let
    appLoop = do
      events <- pollEvents
      let eventIsQPress event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                keyboardEventKeyMotion keyboardEvent == Pressed &&
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
              _ -> False
          qPressed = any eventIsQPress events
      rendererDrawColor renderer $= V4 0 0 255 255
      clear renderer
      ifor_ chunkTextures $ \i texture ->
        copy renderer texture Nothing (Just (Rectangle (P (V2 (0x80 * fromIntegral i) 0)) 0x80))
      present renderer
      unless qPressed appLoop
  appLoop
