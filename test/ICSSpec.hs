module ICSSpec where

import           Data.List.Extra

import           Test.Hspec
import           Test.QuickCheck

import           ICS

legitimateLines :: [String] -> Bool
legitimateLines = all (not . (" " `isPrefixOf`))

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
        let isShortLine ln = counterexample ln (length ln <= 73)
         in map isShortLine (wrapLines lns)
    it "wraps long lines" $ do
      wrapLines [replicate 100 'x'] `shouldBe`
        [replicate 72 'x', " " ++ replicate 28 'x']
      wrapLines [replicate 200 'x'] `shouldBe`
        [replicate 72 'x', " " ++ replicate 72 'x', " " ++ replicate 56 'x']
  describe "unwrapLines" $
    it "is an inverse of wrapLines" $
    property $ \lns ->
      legitimateLines lns ==> unwrapLines (wrapLines lns) === lns
