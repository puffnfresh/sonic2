{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens            (lens)
import           Control.Monad           (unless)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader
import qualified Data.ByteString         as BS
import           Foreign.Marshal.Alloc   (allocaBytes)
import           Foreign.Marshal.Array   (copyArray)
import           Game.Sega.Sonic.Game    (HasRom (..), loadSineData)
import qualified Game.Sega.Sonic.Musashi as Musashi
import           Game.Sega.Sonic.Sine
import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import           Musashi
import           System.Exit             (exitFailure)

prop_calcSine :: SineData -> Property
prop_calcSine sineData =
  property $ do
    n <- forAll $ Gen.word8 Range.linearBounded
    let a = runReader (calcSine n) sineData
    b <- liftIO $ Musashi.calcSine n
    a === b

main :: IO ()
main = do
  bs <- BS.readFile "sonic2.md"
  allocaBytes 0xffffff $ \p -> do
    musashiSetRom p
    BS.useAsCStringLen bs (uncurry (copyArray p))
    musashiInit
    let sineData = runReader loadSineData bs

    result <- checkParallel $ Group "Test.Musashi" [
        ("prop_calcSine", prop_calcSine sineData)
      ]

    unless result exitFailure
