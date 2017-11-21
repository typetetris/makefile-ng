{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Attoparsec.Text
import Data.Makefile
import qualified Data.Makefile.Parse.Internal as PM

simpleAEqualb :: Either a Entry
simpleAEqualb = Right (VariableAssignment (VariableName "a") Recursive (VariableValue "b") (Comment ""))

basicTextParsing :: SpecWith ()
basicTextParsing  =
  describe "basic make text parsing" $
    it "parses \"abc\" as a standard make text" $
      parseOnly (PM.standardMakeTextChunk "#") "abc" `shouldBe` (Right "abc")

variableAssignment :: SpecWith ()
variableAssignment =
  describe "Basic Variable Assignment" $ do
    it "parses a = b\\n" $
      parseOnly PM.variableAssignment
      "a = b\n" `shouldBe` simpleAEqualb
    it "parses a \\\n    = b\\n" $
      parseOnly PM.variableAssignment
      "a \\\n   = b\n" `shouldBe` simpleAEqualb
    it "parses a \\\n  = \\\nb\\n" $
      parseOnly PM.variableAssignment
      "a \\\n   = \\\nb\n" `shouldBe` simpleAEqualb
    it "parses a \\\n  = \\\n\tb\\n" $
      parseOnly PM.variableAssignment
      "a \\\n   = \\\n\tb\n" `shouldBe` simpleAEqualb

rule :: SpecWith ()
rule =
  describe "Parsing of rules" $ do
    it "parses a:\n" $
      parseOnly PM.simpleRule
      "a:\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="", orderOnly=""}) [] (Comment ""))
    it "parses a: b\n" $
      parseOnly PM.simpleRule
      "a: b\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly=""}) [] (Comment ""))
    it "parses a: b|c\n" $
      parseOnly PM.simpleRule
      "a: b|c\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) [] (Comment ""))
    it "parses a: b|c#Muhaha\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) [] (Comment "Muhaha"))
    it "parses a: b|c#Muhaha\n\tfirst recipe line\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) [RecipeLine "first recipe line"] (Comment "Muhaha"))
    it "parses a: b|c#Muhaha\n\tfirst recipe line\n\n\tsecond recipe line after empty line\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\n\n\tsecond recipe line after empty line\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) (map RecipeLine ["first recipe line", "second recipe line after empty line"]) (Comment "Muhaha"))
    it "parses a: b|c#Muhaha\n\tfirst recipe line\n \t   \n\tsecond recipe line after whitespace line\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\n \t   \n\tsecond recipe line after whitespace line\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) (map RecipeLine ["first recipe line", "second recipe line after whitespace line"]) (Comment "Muhaha"))
    it "parses a: b|c#Muhaha\n\tfirst recipe line\n#comment stuff on line sadly lost for now  \n\tsecond recipe line after whitespace line\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\n#comment stuff on line sadly lost for now  \n\tsecond recipe line after comment only line\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) (map RecipeLine ["first recipe line", "second recipe line after comment only line"]) (Comment "Muhaha"))

main :: IO()
main = hspec $ do
  basicTextParsing
  variableAssignment
  rule
