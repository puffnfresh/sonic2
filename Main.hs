{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import qualified Codec.Compression.Kosinski     as Kosinski
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Array.Bounded
import           Data.Array.Bounded             ((!))
import           Data.Bits
import qualified Data.ByteString                as BS
import           Data.ByteString.Lens
import           Data.Halves                    (collectHalves)
import           Data.Int
import           Data.Maybe                     (fromMaybe)
import           Data.Semigroup                 ((<>))
import           Data.Time                      (diffUTCTime, getCurrentTime)
import           Data.Word
import           Foreign.C.Types                (CInt)
import           Game.Sega.Sonic.Animation
import           Game.Sega.Sonic.Blocks
import           Game.Sega.Sonic.Chunks
import           Game.Sega.Sonic.Collision
import           Game.Sega.Sonic.Error
import           Game.Sega.Sonic.Game
import           Game.Sega.Sonic.Layout
import           Game.Sega.Sonic.Offsets        as Offsets
import           Game.Sega.Sonic.Palette
import           Game.Sega.Sonic.Player
import           Game.Sega.Sonic.Sine
import           Game.Sega.Sonic.SpriteMappings
import           Game.Sega.Sonic.Sprites
import           Game.Sega.Sonic.Tiles
import           SDL
import           Sega.MegaDrive.Palette

import           Numeric
import           Debug.Trace

decompressFile :: (HasRom g, MonadReader g m, MonadError SonicError m, MonadIO m) => Offset -> m BS.ByteString
decompressFile offset = do
  maybeContent <- Kosinski.compressed <$> sliceRom offset
  content <- maybe (throwError $ SonicLoadError offset) pure maybeContent
  maybe (throwError $ SonicDecompressionError offset) pure $ Kosinski.decompress content

-- NTSC
frameRate :: Double
frameRate =
  60

class HasLevel s where
  layout :: s -> [[Word8]]
  chunkBlocks :: s -> BoundedArray Word8 (BoundedArray Word8 ChunkBlock)

data LevelData
  = LevelData AngleData [[Word8]] (BoundedArray Word8 (BoundedArray Word8 ChunkBlock))

instance HasAngleData LevelData where
  angleData (LevelData a _ _) =
    a

instance HasLevel LevelData where
  layout (LevelData _ a _) =
    a
  chunkBlocks (LevelData _ _ a) =
    a

findTile :: (HasLevel s, MonadReader s m) => V2 CInt -> m Word16
findTile p = do
  layout' <- asks layout
  let
    p' =
      p ^. pixels
    V2 layoutX layoutY =
      (`div` 0x80) <$> p'
    chunkIndex =
      layout' !! fromIntegral layoutY !! fromIntegral layoutX
    blockIndex =
      fromIntegral chunkIndex * (0x80 :: Word16)
        + fromIntegral (p' ^. _y .&. 0x70)
        + fromIntegral (p' ^. _x .&. 0xE)
  pure blockIndex

data WallDist
  = WallDist Word16 CInt Word8
  deriving (Eq, Ord, Show)

-- Scans horizontally for up to 2 16x16 blocks to find solid walls.
-- d2 = y_pos
-- d3 = x_pos
-- d5 = ($c,$d) or ($e,$f) - solidity type bit (L/R/B or top)
-- d6 = $0000 for no flip, $0400 for horizontal flip
-- a3 = delta-x for next location to check if current one is empty
-- a4 = pointer to angle buffer
-- returns relevant block ID in (a1)
-- returns distance to left/right in d1
-- returns angle in (a4)
findWall :: (HasLevel s, MonadReader s m) => V2 CInt -> CInt -> m WallDist
findWall p delta = do
  blockIndex <- findTile p
  if blockIndex .&. 0x3FF == 0
  then do
    WallDist a1 d1 a4 <- findWall2 (p & _x +~ delta)
    -- dist & distance +~ 0x10
    pure $ WallDist a1 (d1 + 0x10) a4
  else do
    let
      d1 =
        0
      a4 =
        0
    pure $ WallDist blockIndex d1 a4

findWall2 :: (HasLevel s, MonadReader s m) => V2 CInt -> m WallDist
findWall2 p = do
  blockIndex <- (.&. 0x3FF) <$> findTile p
  if blockIndex == 0
  then do
    let
      d1 =
        0xF - (p ^. _x .&. 0xF)
    pure $ WallDist blockIndex d1 0
  else do
    let
      d1 =
        0
      a4 =
        0
    pure $ WallDist blockIndex d1 a4

findFloor :: (HasLevel s, MonadReader s m) => V2 CInt -> CInt -> m WallDist
findFloor p delta = do
  blockIndex <- (.&. 0x3FF) <$> findTile p
  let d5 = 0xC
  if blockIndex == 0 || not (testBit blockIndex d5)
  then do
    WallDist a1 d1 a4 <- findFloor2 (p & _y +~ delta)
    -- dist & distance +~ 0x10
    pure $ WallDist a1 (d1 + 0x10) a4
  else do
    let
      d1 =
        0
      a4 =
        0
    pure $ WallDist blockIndex d1 a4

findFloor2 :: (HasLevel s, MonadReader s m) => V2 CInt -> m WallDist
findFloor2 p = do
  blockIndex <- (.&. 0x3FF) <$> findTile p
  let d5 = 0xC
  if blockIndex == 0 || not (testBit blockIndex d5)
  then do
    let
      d1 =
        0xF - (p ^. _y .&. 0xF)
    pure $ WallDist blockIndex d1 0
  else do
    let
      d1 =
        0
      a4 =
        0
    pure $ WallDist blockIndex d1 a4

-- Checks a 16x16 block to find solid walls. May check an additional
-- 16x16 block up for walls.
-- d5 = ($c,$d) or ($e,$f) - solidity type bit (L/R/B or top)
-- returns relevant block ID in (a1)
-- returns distance in d1
-- returns angle in d3, or zero if angle was odd
checkLeftWallDist :: (HasLevel r, MonadReader r m, HasPlayer s, MonadState s m) => m WallDist
checkLeftWallDist = do
  p <- use (player . position)
  findWall p (-0x10)

checkRightWallDist :: (HasLevel r, MonadReader r m, HasPlayer s, MonadState s m) => m WallDist
checkRightWallDist = do
  p <- use (player . position)
  findWall p (0x10)

checkCeiling :: (HasLevel r, MonadReader r m, HasPlayer s, MonadState s m) => m WallDist
checkCeiling = do
  p <- use (player . position)
  findFloor p (-0x10)

checkFloor :: (HasLevel r, MonadReader r m, HasPlayer s, MonadState s m) => m WallDist
checkFloor = do
  p <- use (player . position)
  findFloor p 0x10

hitLeftWall :: (HasLevel r, MonadReader r m, HasPlayer s, MonadState s m) => m ()
hitLeftWall = do
  WallDist _ d1 _ <- checkLeftWallDist
  traceShow ("hitLeftWall", d1) $ pure ()
  if d1 >= 0
  then hitCeiling
  else do
    player . position . _x -= d1
    player . playerVelocity . _x .= 0
    y_vel <- use (player . playerVelocity . _y)
    player . playerInertia .= y_vel

hitCeiling :: (HasLevel g, MonadReader g m, HasPlayer s, MonadState s m) => m ()
hitCeiling = do
  WallDist _ d1 _ <- checkCeiling
  traceShow ("hitCeiling", d1) $ pure ()
  if d1 >= 0
  then hitFloor
  else do
    player . position . _y -= d1
    y_vel <- use (player . playerVelocity . _y)
    when (y_vel < 0) $
      player . playerVelocity . _y .= 0

hitFloor :: (HasLevel g, MonadReader g m, HasPlayer s, MonadState s m) => m ()
hitFloor = do
  y_vel <- use (player . playerVelocity . _y)
  unless (y_vel < 0) $ do
    WallDist _ d1 d3 <- checkFloor
    traceShow ("hitFloor", d1) $ pure ()
    when (d1 < 0) $ do
      player . position . _y += d1
      player . playerAngle .= d3
      resetOnFloor
      player . playerVelocity . _y .= 0
      x_vel <- use (player . playerVelocity . _x)
      player . playerInertia .= x_vel

hitCeilingAndWalls :: (HasLevel g, MonadReader g m, HasPlayer s, MonadState s m) => m ()
hitCeilingAndWalls = do
  WallDist _ d1 _ <- checkLeftWallDist
  when (d1 < 0) $ do
    player . position . _x -= d1
    player . playerVelocity . _x .= 0

  WallDist _ d1' _ <- checkRightWallDist
  when (d1' < 0) $ do
    player . position . _x += d1
    player . playerVelocity . _x .= 0

  WallDist _ d1'' d3 <- checkCeiling
  when (d1'' < 0) $ do
    player . position . _y -= d1''
    let d0 = (d3 + 0x20) .&. 0x40
    if (d0 /= 0)
    then do
      player . playerAngle .= d3
      resetOnFloor
      y_vel <- use (player . playerVelocity . _y)
      player . playerInertia .= y_vel
      inertia <- use (player . playerInertia)
      unless (d3 < 0) $
        player . playerInertia .= (-inertia)
    else player . playerVelocity . _y .= 0;

hitRightWall :: (HasLevel g, MonadReader g m, HasPlayer s, MonadState s m) => m ()
hitRightWall = do
  WallDist _ d1 _ <- checkRightWallDist
  traceShow ("hitRightWall", d1) $ pure ()
  if d1 >= 0
  then hitCeiling
  else do
    player . position . _x += d1
    player . playerVelocity . _x .= 0
    y_vel <- use (player . playerVelocity . _y)
    player . playerInertia .= y_vel

doLevelCollision :: (HasAngleData g, HasLevel g, MonadReader g m, HasPlayer s, MonadState s m) => m ()
doLevelCollision = do
  -- TODO: Check left/right/bottom solid bit
  v <- use (player . playerVelocity)
  a <- calcAngle v
  traceShow ("doLevelCollision", (a - 0x20) .&. 0xC0) $ pure ()
  case (a - 0x20) .&. 0xC0 of
    0x40 -> hitLeftWall
    0x80 -> hitCeilingAndWalls
    0xC0 -> hitRightWall
    _    -> do
      WallDist _ d1 _ <- checkLeftWallDist
      when (d1 < 0) $ do
        -- p->x_pos -= d1;
        -- p->x_vel = 0;
        traceShow "TODO" $ pure ()

      WallDist _ d1' _ <- checkLeftWallDist
      when (d1' < 0) $ do
        -- p->x_pos += d1;
        -- p->x_vel = 0;
        traceShow "TODO" $ pure ()

      WallDist _ d1'' d3 <- checkFloor
      unless (d1'' < 0) $ do
        v <- use (player . playerVelocity)
        -- let d2 = negate ((v ^. _y `shiftR` 8) + 8)
        player . position . _y += d1''
        player . playerAngle .= d3
        resetOnFloor
        -- d0 = (d3 + 0x10) & 0x20;
        -- if((char)d0 == 0)
        -- 	goto loc_1AF5A;
        -- if(p->y_vel < 0){	// p->y_vel /= 2; -- DONE THIS WAY FOR ACCURACY
        -- 	p->y_vel >>= 1;
        -- 	p->y_vel |= 0x80000000;
        -- }
        -- else
        -- 	p->y_vel >>= 1;
        -- goto loc_1AF7C;

-- Subroutine to change Sonic's angle as he walks along the floor
sonicAngle :: (Applicative m) => Word8 -> m ()
sonicAngle _ =
  pure ()

anglePos :: (HasLevel g, HasAngleData g, MonadReader g m, HasPlayer s, MonadState s m) => m ()
anglePos = do
  -- unless onobject
  a <- use (player . playerAngle)
  let
    a' =
      if a + 0x20 >= 0
      then (if a < 0 then a + 1 else a) + 0x1F
      else error "angle pos 1"

  case (a' + 0x20) .&. 0xC0 of
    0x40 -> pure ()
    0x80 -> pure ()
    0xC0 -> pure ()
    _    -> do
      p <- use (player . position)
      r <- use (player . playerRadius)
      WallDist _ d1 a4 <- findFloor (p + r) 0x10
      sonicAngle a4
      unless (d1 == 0) $ do
        if d1 >= 0
        then do
          v <- use (player . playerVelocity)
          let d0 = min 0xE (abs (v ^. _x `shiftR` 8) + 4)
          if d1 > fromIntegral d0
          then player . statuses . mdAir .= MdAirOn
            -- player . status &= 0xDF
            -- pure ()
          else player . position . _y += d1
        else
          unless (d1 < (-0xE)) $
            player . position . _y += d1

x :: (HasLevel g, MonadReader g m) => V2 CInt -> m WallDist
x p = do
  l <- asks layout
  c <- asks chunkBlocks
  let
    reindexedCollisionBlocks =
      undefined
    reindexedCurves =
      undefined

    V2 layoutX layoutY =
      (`div` 0x80) <$> p
    chunkIndex =
      l !! fromIntegral layoutY !! fromIntegral layoutX
    V2 blockX blockY =
      ((`div` 0x10) . (`rem` 0x80)) <$> p
    ChunkBlock blockIndex flipX flipY =
      (c ! chunkIndex) ! fromIntegral ((blockY * 8) + blockX)
    V2 pixelX pixelY =
      (`rem` 0x10) <$> p
    CollisionBlock heights =
      reindexedCollisionBlocks ! blockIndex
    angle' =
      (if flipX then negate else id) $ reindexedCurves ! blockIndex
    flip' flag n =
      if flag then 0xF - n else n
    height =
      fromMaybe 0 (heights ! fromIntegral (flip' flipX pixelX))
    heightDifference =
      (0x10 - flip' flipY pixelY) - (fromIntegral height + 2)
  pure $ WallDist blockIndex heightDifference angle'

loadAndRun :: (MonadReader Game m, MonadError SonicError m, MonadIO m) => m ()
loadAndRun = do
  sonicMappings <- loadSpriteMappings sonicOffsets
  tailsMappings <- loadSpriteMappings tailsOffsets

  curves <- listArrayFill 0 . BS.unpack <$> sliceRom curveAndResistanceMapping

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

  collisionIndexContent <- decompressFile $ levelCollisionOffset offsets
  collisionIndex <- loadCollisionIndex collisionIndexContent

  collisionContent <- sliceRom collisionArray1
  let collisionBlocks = loadCollisionBlocks collisionContent
  collisionBlockTextures <- traverse loadCollisionTexture collisionBlocks

  let reindexedCollisionTextures = (collisionBlockTextures !) <$> collisionIndex
      reindexedCollisionBlocks = (collisionBlocks !) <$> collisionIndex
      reindexedCurves = (curves !) <$> collisionIndex

  now <- liftIO getCurrentTime
  chunksContent <- decompressFile $ levelChunksOffset offsets
  liftIO $ putStrLn "Loading chunks..."
  let chunksBlocks = loadChunks chunksContent
  chunksTextures <- traverse (loadChunkTexture reindexedCollisionTextures) chunksBlocks
  now' <- liftIO getCurrentTime
  liftIO . putStrLn $ "Chunks loaded in " <> show (diffUTCTime now' now)

  sineData' <- SineData . listArrayFill 0 . fmap fromIntegral . view (unpackedBytes . collectHalves) <$> sliceRom Offsets.sineData
  angleData' <- AngleData . listArrayFill 0 . fmap fromIntegral . view (unpackedBytes . collectHalves) <$> sliceRom Offsets.angleData

  let
    collisionTextures = mapChunkTextures chunksTextures layout
    levelData = LevelData angleData' layout chunkBlocks

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
    render textures (V2 o p) =
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
        jumpPressed =
          isPressed KeycodeA || isPressed KeycodeS || isPressed KeycodeD
        leftPressed =
          isPressed KeycodeLeft
        rightPressed =
          isPressed KeycodeRight
        -- downPressed =
        --   isPressed KeycodeDown
        -- upPressed =
        --   isPressed KeycodeUp
        playerSprite'' =
          playerSprite' & position .~ (fromIntegral <$> (game ^. player . position . pixels))
        updateGame = do
          zoom player $ do
            s <- use statuses
            traceShow (playerRoutine s) $ pure ()
            case playerRoutine s of
              MdNormal -> do
                if jumpPressed
                then runReaderT jump sineData'
                else do
                  if rightPressed
                  then moveRight
                  else when leftPressed moveLeft
                  when (not rightPressed && not leftPressed) settle
                  objectMove
                  traction
                  runReaderT anglePos levelData
              MdAir -> do
                objectMoveAndFall
                runReaderT doLevelCollision levelData
              MdRoll ->
                pure ()
              MdJump -> do
                objectMoveAndFall
                runReaderT doLevelCollision levelData
          p' <- use (player . position . pixels)
          camera .= (fromIntegral <$> p') - V2 160 128 -- V2 o' p'
        game' =
          execState updateGame game

      rendererDrawColor r $= V4 0 0 0 0xFF
      clear r
      render layoutChunkTextures (game' ^. camera)
      runReaderT (renderSprite playerSprite'') game'
      present r
      delay 16
      unless qPressed (appLoop (stepSprite playerSprite'') game')
  game <- ask
  appLoop playerSprite (game & player . position . pixels .~ playerStart)

main :: IO ()
main = do
  rom' <- BS.readFile "sonic2.md"

  window <- createWindow "Sonic 2" defaultWindow { windowInitialSize = V2 320 224 }
  renderer' <- createRenderer window (-1) defaultRenderer
  rendererLogicalSize renderer' $= Just (V2 320 224)

  e <- runReaderT (runExceptT loadAndRun) (Game renderer' 0 rom' $ Player (V2 0 0) (V2 0 0) (V2 0 0x13) normalTopSpeed normalAcceleration normalDeceleration 0 0 initialStatuses)
  either print pure e
