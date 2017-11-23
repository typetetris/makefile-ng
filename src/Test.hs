{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Makefile
import Data.Either (isLeft)
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
    it "should fail on '  a  b = c \n'" $
      parseOnly PM.variableAssignment
      "  a  b = c \n" `shouldSatisfy` isLeft

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
    it "parses a: b|c#Muhaha\n\tfirst recipe line\\\n\twith a line continuation\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\\\n\twith a line continuation\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) [RecipeLine "first recipe line\\\nwith a line continuation"] (Comment "Muhaha"))
    it "parses a: b|c#Muhaha\n\tfirst recipe line\n\n\tsecond recipe line after empty line\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\n\n\tsecond recipe line after empty line\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) (map RecipeLine ["first recipe line", "second recipe line after empty line"]) (Comment "Muhaha"))
    it "parses a: b|c#Muhaha\n\tfirst recipe line\n \t   \n\tsecond recipe line after whitespace line\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\n \t   \n\tsecond recipe line after whitespace line\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) (map RecipeLine ["first recipe line", "second recipe line after whitespace line"]) (Comment "Muhaha"))
    it "parses a: b|c#Muhaha\n\tfirst recipe line\n#comment stuff on line sadly lost for now  \n\tsecond recipe line after whitespace line\n" $
      parseOnly PM.simpleRule
      "a: b|c#Muhaha\n\tfirst recipe line\n#comment stuff on line sadly lost for now  \n\tsecond recipe line after comment only line\n" `shouldBe` Right (SimpleRule (Target "a") (Dependencies{normal="b", orderOnly="c"}) (map RecipeLine ["first recipe line", "second recipe line after comment only line"]) (Comment "Muhaha"))
    it "parses a: b ; do something\n" $
      parseOnly PM.simpleRule "a: b ; do something\n" `shouldBe`
           Right (SimpleRule (Target "a") Dependencies{normal = "b ", orderOnly = ""} [RecipeLine " do something"] (Comment ""))
    it "parses a: b ; do something # comment on inlineRecipeLine\n" $
      parseOnly PM.simpleRule "a: b ; do something # comment on inlineRecipeLine\n" `shouldBe`
           Right (SimpleRule (Target "a") Dependencies{normal = "b ", orderOnly = ""} [RecipeLine " do something # comment on inlineRecipeLine"] (Comment ""))
    it "parses a: b ; do something # comment on inlineRecipeLine\n\tsecond Recipe Line # with comment\n" $
      parseOnly PM.simpleRule "a: b ; do something # comment on inlineRecipeLine\n\tsecond Recipe Line # with comment\n" `shouldBe`
           Right (SimpleRule (Target "a") Dependencies{normal = "b ", orderOnly = ""} [RecipeLine " do something # comment on inlineRecipeLine", RecipeLine "second Recipe Line # with comment"] (Comment ""))

smallMakefileResult :: Makefile
smallMakefileResult = Makefile
  [ CommentLine (Comment " A first comment line")
  , SimpleRule (Target "a") Dependencies{normal="b c ", orderOnly=""} [RecipeLine "With a recipe line"] (Comment " a little rule")
  , PatternRule (Target "%.o") Dependencies{normal="%.c ", orderOnly=""} [RecipeLine "${LD} ${LDFLAGS} $< -o $@"] (Comment " little pattern rule")
  , StaticPatternRule (Target "a.t b.t c.t ") (Target " %.t ") Dependencies{normal="%.v ", orderOnly=" f "} [RecipeLine "möp"] (Comment " wow a static pattern rule with a order only dependency, look at that!")
  , VariableAssignment (VariableName "${wtf}") Conditional (VariableValue "${wtf2} ") (Comment " comment on that boy!")
  ]

smallMakefile :: Text
smallMakefile = "\
\  # A first comment line\n\
\a: b c # a little rule\n\
\\tWith a recipe line\n\
\%.o: %.c # little pattern rule\n\
\\t${LD} ${LDFLAGS} $< -o $@\n\
\a.t b.t c.t : %.t : %.v | f # wow a static pattern rule with a order only dependency, look at that!\n\
\\tmöp\n\
\${wtf} ?= ${wtf2} # comment on that boy!\n"


makefile :: SpecWith ()
makefile = describe "test makefile parsing" $
  it "parses a little makefile" $
    parseOnly PM.makefile smallMakefile `shouldBe` Right smallMakefileResult

main :: IO()
main = hspec $ do
  basicTextParsing
  variableAssignment
  rule
  makefile
