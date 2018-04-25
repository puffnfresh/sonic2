{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens              (ifor_)
import           Control.Monad             (unless)
import qualified Data.ByteString           as BS
import           Game.Sega.Sonic.Collision (loadCollisionTextures)
import           SDL
import           System.FilePath.Posix     ((</>))

main :: IO ()
main = do
  window <- createWindow "hsonic 2" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  collisionsContent <- BS.readFile ("s2disasm" </> "collision" </> "Collision array 1.bin")
  collisionTextures <- loadCollisionTextures renderer collisionsContent
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
      ifor_ collisionTextures $ \i texture ->
        copy renderer texture Nothing (Just (Rectangle (P (V2 (0x10 * fromIntegral i) 0)) 0x10))
      present renderer
      unless qPressed appLoop
  appLoop
