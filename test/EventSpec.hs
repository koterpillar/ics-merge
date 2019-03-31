{-# LANGUAGE OverloadedStrings #-}

module EventSpec where

import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Arbitrary ()

import           Test.Hspec
import           Test.QuickCheck

import           Event

legitimateLines :: [Text] -> Bool
legitimateLines = all (not . (" " `Text.isPrefixOf`))

spec :: Spec
spec = do
  describe "wrapLines" $ do
    it "acts on each line independently" $
      property $ \lns ->
        legitimateLines lns ==> wrapLines lns ===
        concatMap (\ln -> wrapLines [ln]) lns
    it "produces short lines" $
      property $ \lns ->
        legitimateLines lns ==> conjoin $
        let isShortLine ln =
              counterexample (Text.unpack ln) (Text.length ln <= 73)
         in map isShortLine (wrapLines lns)
    it "wraps long lines" $ do
      wrapLines [Text.replicate 100 "x"] `shouldBe`
        [Text.replicate 72 "x", " " <> Text.replicate 28 "x"]
      wrapLines [Text.replicate 200 "x"] `shouldBe`
        [ Text.replicate 72 "x"
        , " " <> Text.replicate 72 "x"
        , " " <> Text.replicate 56 "x"
        ]
  describe "unwrapLines" $
    it "is an inverse of wrapLines" $
    property $ \lns ->
      legitimateLines lns ==> unwrapLines (wrapLines lns) === lns
