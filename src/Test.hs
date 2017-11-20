{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Attoparsec.Text
import Data.Makefile
import Data.Makefile.Parse.Internal

main :: IO()
main = hspec $ do
  describe "basic make text parsing" $
    it "parses \"abc\" as a standard make text" $
      parseOnly (standardMakeTextChunk "#") "abc" `shouldBe` (Right "abc")
  describe "Basic Variable Assignment" $
    it "parses a = b\\n" $
      parseOnly variableAssignment "a = b\n" `shouldBe` Right (VariableAssignment (VariableName "a") Recursive (VariableValue "b") (Comment ""))
