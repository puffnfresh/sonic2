{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Collision (
  collisionHeight
, collisionPixel
, loadCollisionTexture
, loadCollisionTextures
, loadCollisionIndex
) where

import           Control.Applicative     (liftA2)
import           Control.Lens            (ix, (^?))
import           Control.Monad.Except    (MonadError, throwError)
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Array.Bounded
import           Data.Bits               ((.&.))
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (toLazyByteString, word32LE)
import qualified Data.ByteString.Lazy    as BSL
import           Data.List.NonEmpty      (NonEmpty (..), nonEmpty)
import           Data.List.Split         (chunksOf)
import           Data.Word               (Word16, Word32, Word8)
import           Game.Sega.Sonic.Error   (SonicError (..))
import           SDL

collisionHeight :: Word8 -> Maybe Word8
collisionHeight n =
  let h = 0x1F .&. n
  in if h > 0 then Just (h - 1) else Nothing

collisionPixel :: Maybe Word8 -> Word8 -> Word32
collisionPixel h y =
  if maybe True (< fromIntegral (0xF - y)) h
  then 0xFFFFFFFF
  else 0xFF000000 :: Word32

-- | <http://stephenuk.hacking-cult.org/SCHG/General/CollisionFormat/CollisionFormat.htm SCHG page on the collision format>
loadCollisionTexture :: (MonadIO m) => Renderer -> [Word8] -> m Texture
loadCollisionTexture renderer s = do
  texture <- createTexture renderer ABGR8888 TextureAccessStatic 0x10
  let
    height x =
      s ^? ix x >>= collisionHeight
    heights =
      listArrayCycle $ height <$> (0 :| [1..])
    pixel y x =
      word32LE $ collisionPixel (heights ! x) y
    content =
      foldMap (uncurry pixel) $ liftA2 (,) [0..0xF] [0..0xF :: Word8]
  updateTexture texture Nothing (BSL.toStrict $ toLazyByteString content) (4 * 0x10)

loadCollisionTextures :: (MonadIO m) => Renderer -> BS.ByteString -> m (BoundedArray Word8 Texture)
loadCollisionTextures renderer =
  traverse (loadCollisionTexture renderer) . listArrayFill [] . chunksOf 0x10 . BS.unpack

loadCollisionIndex :: (MonadError SonicError m) => BS.ByteString -> m (BoundedArray Word16 Word8)
loadCollisionIndex c = do
  xs <- maybe (throwError SonicEmptyCollisionIndexError) pure . nonEmpty $ BS.unpack c
  pure $ listArrayCycle xs
