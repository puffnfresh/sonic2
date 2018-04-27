module Game.Sega.Sonic.Collision (
  collisionHeight
, collisionPixel
, loadCollisionTexture
, loadCollisionTextures
, loadCollisionIndex
) where

import           Control.Applicative     (liftA2)
import           Control.Lens            (ix, (^?))
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Array              (Array, listArray, (!))
import           Data.Bits               ((.&.))
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (toLazyByteString, word32LE)
import qualified Data.ByteString.Lazy    as BSL
import           Data.List.Split         (chunksOf)
import           Data.Word               (Word32, Word16, Word8)
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
      listArray (0, 0xF :: Word8) $ height <$> [0..]
    pixel y x =
      word32LE $ collisionPixel (heights ! x) y
    content =
      foldMap (uncurry pixel) $ liftA2 (,) [0..0xF] [0..0xF]
  updateTexture texture Nothing (BSL.toStrict $ toLazyByteString content) (4 * 0x10)

loadCollisionTextures :: (MonadIO m) => Renderer -> BS.ByteString -> m (Array Word8 Texture)
loadCollisionTextures renderer =
  fmap (listArray (0, 0xFF)) . traverse (loadCollisionTexture renderer) . chunksOf 0x10 . BS.unpack

loadCollisionIndex :: BS.ByteString -> Array Word16 Word8
loadCollisionIndex =
  listArray (0, 0x2FF) . BS.unpack
