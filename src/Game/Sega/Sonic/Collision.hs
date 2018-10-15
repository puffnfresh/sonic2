{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Sega.Sonic.Collision (
  CollisionBlock(..)
, collisionHeight
, collisionBitmap
, loadCollisionTexture
, loadCollisionBlocks
, loadCollisionIndex
) where

import           Control.Applicative     (liftA2)
import           Control.Lens            (ix, view, (^?))
import           Control.Monad.Except    (MonadError, throwError)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (MonadReader)
import           Data.Array.Bounded
import           Data.Bits               ((.&.))

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (toLazyByteString, word32LE)
import qualified Data.ByteString.Lazy    as BSL
import           Data.List.NonEmpty      (NonEmpty (..), nonEmpty)
import           Data.List.Split         (chunksOf)
import           Data.Maybe              (fromMaybe)
import           Data.Word               (Word16, Word32, Word8)
import           Game.Sega.Sonic.Error   (SonicError (..))
import           Game.Sega.Sonic.Game    (HasRenderer (..))
import           SDL

data CollisionBlock
  = CollisionBlock (BoundedArray Word8 (Maybe Word8))
  deriving (Show, Eq, Ord)

collisionHeight :: Word8 -> Maybe Word8
collisionHeight n =
  let h = 0x1F .&. n
  in if h > 0 then Just (h - 1) else Nothing

collisionBitmap :: CollisionBlock -> BoundedArray Word8 Bool
collisionBitmap (CollisionBlock heights) =
  let
    testPixel h y =
      maybe False (>= fromIntegral (0xF - y)) h
    pixel y x =
      testPixel (heights ! x) y
    content =
      liftA2 pixel [0..0xF :: Word8] [0..0xF :: Word8]
  in listArrayFill False content

-- | <http://stephenuk.hacking-cult.org/SCHG/General/CollisionFormat/CollisionFormat.htm SCHG page on the collision format>
loadCollisionTexture :: (HasRenderer g, MonadReader g m, MonadIO m) => CollisionBlock -> m Texture
loadCollisionTexture s = do
  r <- view renderer
  texture <- createTexture r ABGR8888 TextureAccessStatic 0x10
  let
    collisionPixel p =
      word32LE $ if p then 0xFFFFFFFF else 0xFF000000 :: Word32
    content =
      foldMap collisionPixel $ collisionBitmap s
  updateTexture texture Nothing (BSL.toStrict $ toLazyByteString content) (4 * 0x10)

loadCollisionBlock :: [Word8] -> CollisionBlock
loadCollisionBlock s =
  let
    height x =
      s ^? ix x >>= collisionHeight
    heights =
      listArrayCycle $ height <$> (0 :| [1..])
  in CollisionBlock heights

loadCollisionBlocks :: BS.ByteString -> BoundedArray Word8 CollisionBlock
loadCollisionBlocks =
  fmap loadCollisionBlock . listArrayFill [] . chunksOf 0x10 . BS.unpack

loadCollisionIndex :: (MonadError SonicError m) => BS.ByteString -> m (BoundedArray Word16 Word8)
loadCollisionIndex c = do
  xs <- maybe (throwError SonicEmptyCollisionIndexError) pure . nonEmpty $ BS.unpack c
  pure $ listArrayCycle xs
