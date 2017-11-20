{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Attoparsec.Text
import Data.Makefile
import Data.Makefile.Parse.Internal

simpleAEqualb :: Either a Entry
simpleAEqualb = Right (VariableAssignment (VariableName "a") Recursive (VariableValue "b") (Comment ""))


main :: IO()
main = hspec $ do
  describe "basic make text parsing" $
    it "parses \"abc\" as a standard make text" $
      parseOnly (standardMakeTextChunk "#") "abc" `shouldBe` (Right "abc")
  describe "Basic Variable Assignment" $ do
    it "parses a = b\\n" $
      parseOnly variableAssignment
      "a = b\n" `shouldBe` simpleAEqualb
    it "parses a \\\n    = b\\n" $
      parseOnly variableAssignment
      "a \\\n   = b\n" `shouldBe` simpleAEqualb
    it "parses a \\\n  = \\\nb\\n" $
      parseOnly variableAssignment
      "a \\\n   = \\\nb\n" `shouldBe` simpleAEqualb
    it "parses a \\\n  = \\\n\tb\\n" $
      parseOnly variableAssignment
      "a \\\n   = \\\n\tb\n" `shouldBe` simpleAEqualb
