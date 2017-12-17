{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Makefile
import Data.Either (isLeft)
import qualified Data.Makefile.Parse.Internal as PM

simpleAEqualb :: Entry
simpleAEqualb = VariableAssignment (VariableName "a") Recursive (VariableValue "b") (Comment "")

varWithColonValue :: Entry
varWithColonValue = VariableAssignment (VariableName "VARNAME") Recursive (VariableValue ":") (Comment "")

basicTextParsing :: SpecWith ()
basicTextParsing  =
  describe "basic make text parsing" $
    it "parses \"abc\" as a standard make text" $
      parseOnly (PM.standardMakeTextChunk "#") "abc" `shouldBe` Right "abc"

data TableTest a = TableTest Text a

vaaTests :: [TableTest Entry]
vaaTests = [TableTest "a = b\n" simpleAEqualb
           ,TableTest "a \\\n    = b\n" simpleAEqualb
           ,TableTest "a \\\n   = \\\nb\n" simpleAEqualb
           ,TableTest "a \\\n   = \\\n\tb\n" simpleAEqualb
           ,TableTest "VARNAME = :\n" varWithColonValue
           ,TableTest "VARNAME =\n" $ VariableAssignment (VariableName "VARNAME") Recursive (VariableValue "") (Comment "")
           ,TableTest "$(VARNAMEINVAR) = value\n" $ VariableAssignment (VariableName "$(VARNAMEINVAR)") Recursive (VariableValue "value") (Comment "")
           ,TableTest "${VARNAMEINVAR} = value\n" $ VariableAssignment (VariableName "${VARNAMEINVAR}") Recursive (VariableValue "value") (Comment "")]

doTableTest :: (Eq a, Show a) => Parser a -> TableTest a -> SpecWith (Arg Expectation)
doTableTest parser (TableTest input output) = it ("parses " ++ T.unpack input) (parseOnly parser input `shouldBe` Right output)

variableAssignment :: SpecWith ()
variableAssignment =
  describe "Basic Variable Assignment" $ do
    it "should fail on '  a  b = c \n'" $
      parseOnly PM.variableAssignment
      "  a  b = c \n" `shouldSatisfy` isLeft
    mapM_ (doTableTest PM.variableAssignment) vaaTests

srTests :: [TableTest Entry]
srTests = [TableTest "a:\n"                                   (SimpleRule (Target "a") Dependencies{normal="", orderOnly=""} [] (Comment ""))
          , TableTest "a: b\n"                                (SimpleRule (Target "a") Dependencies{normal="b", orderOnly=""} [] (Comment ""))
          , TableTest "a: b|c\n"                              (SimpleRule (Target "a") Dependencies{normal="b", orderOnly="c"} [] (Comment ""))
          , TableTest "a: b|c#Muhaha\n"                       (SimpleRule (Target "a") Dependencies{normal="b", orderOnly="c"} [] (Comment "Muhaha"))
          , TableTest "a: b|c#Muhaha\n\tfirst recipe line\n"  (SimpleRule (Target "a") Dependencies{normal="b", orderOnly="c"} [RecipeLine "first recipe line"] (Comment "Muhaha"))
          , TableTest "a: b|c#Muhaha\n\tfirst recipe line\\\n\twith a line continuation\n"
            (SimpleRule (Target "a")
                        Dependencies{normal="b", orderOnly="c"}
                        [RecipeLine "first recipe line\\\nwith a line continuation"]
                        (Comment "Muhaha"))
          , TableTest "a: b|c#Muhaha\n\tfirst recipe line\n\n\tsecond recipe line after empty line\n"
            (SimpleRule (Target "a")
                        Dependencies{normal="b", orderOnly="c"}
                        (map RecipeLine [ "first recipe line"
                                        , "second recipe line after empty line"])
                        (Comment "Muhaha"))
          , TableTest "a: b|c#Muhaha\n\tfirst recipe line\n \t   \n\tsecond recipe line after whitespace line\n"
            (SimpleRule (Target "a")
                        Dependencies{normal="b", orderOnly="c"}
                        (map RecipeLine [ "first recipe line"
                                        , "second recipe line after whitespace line"])
                        (Comment "Muhaha"))
          , TableTest "a: b|c#Muhaha\n\tfirst recipe line\n#comment stuff on line sadly lost for now  \n\tsecond recipe line after comment only line\n"
            (SimpleRule (Target "a")
                        Dependencies{normal="b", orderOnly="c"}
                        (map RecipeLine [ "first recipe line"
                                        , "second recipe line after comment only line"])
                        (Comment "Muhaha"))
          , TableTest "a: b ; do something\n"
            (SimpleRule (Target "a")
                        Dependencies{normal = "b ", orderOnly = ""}
                        [RecipeLine " do something"]
                        (Comment ""))
          , TableTest "a: b ; do something # comment on inlineRecipeLine\n"
            (SimpleRule (Target "a")
                        Dependencies{normal = "b ", orderOnly = ""}
                        [RecipeLine " do something # comment on inlineRecipeLine"]
                        (Comment ""))
          , TableTest "a: b ; do something # comment on inlineRecipeLine\n\tsecond Recipe Line # with comment\n"
            (SimpleRule (Target "a")
                        Dependencies{normal = "b ", orderOnly = ""}
                        [RecipeLine " do something # comment on inlineRecipeLine", RecipeLine "second Recipe Line # with comment"]
                        (Comment ""))
          ]

rule :: SpecWith ()
rule =
  describe "Parsing of rules" $ mapM_ (doTableTest PM.simpleRule) srTests

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

mfTests :: [TableTest Makefile]
mfTests = [TableTest smallMakefile smallMakefileResult
          ,TableTest "VARNAME = :\n" $ Makefile [varWithColonValue]
          ,TableTest "define VARNAME\nline1\nline2\nendef\n" $ Makefile [MultilineVariableAssignment
                                                                                              (VariableName "VARNAME")
                                                                                              Recursive
                                                                                              [ VariableValue "line1"
                                                                                              , VariableValue "line2"
                                                                                              ]
                                                                                              (Comment "")]
          ]


makefile :: SpecWith ()
makefile = describe "test makefile parsing" $ mapM_ (doTableTest PM.makefile) mfTests

utTests :: [TableTest UnevaluatedText]
utTests = [TableTest "  uiae u:=# uiae \\iae \\\\\nuiaed\\duiaeiaedrnxvlczιαλ"  (UnevaluatedText [Plain "  uiae u:=# uiae \\iae \\\\\nuiaed\\duiaeiaedrnxvlczιαλ"])
          ,TableTest "$a"  (UnevaluatedText [VariableReference $ UnevaluatedText [Plain "a"]])
          ,TableTest "$(a)"  (UnevaluatedText [VariableReference $ UnevaluatedText [Plain "a"]])
          ,TableTest "${a}"  (UnevaluatedText [VariableReference $ UnevaluatedText [Plain "a"]])
          ,TableTest "${patsubst a,b,aacc}"  (UnevaluatedText [FunctionCall "patsubst" [UnevaluatedText [Plain "a"]
                                                      ,UnevaluatedText [Plain "b"]
                                                      ,UnevaluatedText [Plain "aacc"]]])
          ]

unevaluatedtext :: SpecWith ()
unevaluatedtext = describe "test text parsing in makefiles" $ do
  mapM_ (doTableTest PM.unevaluatedText) utTests
  it "parses \"${patsubst a,b,aacc}\"" $
    parseOnly PM.utfunctionCall "${patsubst a,b,aacc}" `shouldBe` Right (FunctionCall "patsubst" [UnevaluatedText [Plain "a"]
                                                     ,UnevaluatedText [Plain "b"]
                                                     ,UnevaluatedText [Plain "aacc"]])
  it "parses \"$(patsubst ((,)),(()),((,))aabbcc)\"" $
    parseOnly PM.utfunctionCall "$(patsubst ((,)),(()),((,))aabbcc)" `shouldBe`
      Right (FunctionCall "patsubst" [UnevaluatedText [Plain "((,))"]
                                                     ,UnevaluatedText [Plain "(())"]
                                                     ,UnevaluatedText [Plain "((,))aabbcc"]])
  it "parses \"((,))\"" $
    parseOnly (PM.unevaluatedText' '(' ')' (/= ',') (const True)) "((,))" `shouldBe`
      Right (UnevaluatedText [Plain "((,))"])

main :: IO()
main = hspec $ do
  basicTextParsing
  variableAssignment
  rule
  makefile
  unevaluatedtext
