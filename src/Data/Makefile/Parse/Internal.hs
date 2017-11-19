{-# LANGUAGE OverloadedStrings #-}
module Data.Makefile.Parse.Internal where

import Data.Makefile

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

import Data.Monoid
import Control.Applicative
import Data.Char (isSpace)

data RecipeLineParseState =
    Clear
  | BackslashSeen
  | QuotedNewlineSeen
  deriving (Eq, Show, Enum)

pNC :: (LTB.Builder, RecipeLineParseState) -> Char -> Maybe (LTB.Builder, RecipeLineParseState)
pNC (b, Clear) '\\' = Just (b <> LTB.singleton '\\', BackslashSeen)
pNC (b, BackslashSeen) '\n' = Just (b <> LTB.singleton '\n', QuotedNewlineSeen)
pNC (b, QuotedNewlineSeen) '\t' = Just (b, Clear)
pNC (_, _) '\n' = Nothing
pNC (b, _) ch = Just (b <> LTB.singleton ch, Clear)


recipeLine2 :: Parser Text
recipeLine2 = do
  _ <- P.char '\t'
  (_, (b, _)) <- P.runScanner (LTB.fromText T.empty, Clear) pNC
  return $ LT.toStrict $ LTB.toLazyText b

lineContinuation :: Parser Text
lineContinuation = P.string "\\\n"

recipeLineContinuation :: Parser Text
recipeLineContinuation = P.string "\\\n\t" >> return "\\\n"

recipeLine :: Parser RecipeLine
recipeLine = do
  _ <- P.char '\t'
  rest <- P.many' (recipeLineContinuation <|> lineContinuation <|> P.string "\\" <|> P.takeWhile1 (\c -> c /= '\\' && c /= '\n'))
  _ <- P.char '\n'
  return $ RecipeLine $ T.concat rest

isLineSpace :: Char -> Bool
isLineSpace c = c /= '\n' && isSpace c

lineSpaceCont :: (Parser Text -> Parser [Text]) -> Parser Text
lineSpaceCont f = P.takeWhile isLineSpace >> f (lineContinuation >> P.takeWhile isLineSpace) >> return " "

lineSpace :: Parser Text
lineSpace = lineSpaceCont P.many'


assignOp :: Parser AssignOp
assignOp = (P.string "::=" >> return PosixSimple)
       <|> (P.string ":=" >> return Simple)
       <|> (P.string "?=" >> return Conditional)
       <|> (P.string "!=" >> return Shell)
       <|> (P.string "+=" >> return Append)
       <|> (P.string "=" >> return Recursive)

variableNameChunk :: Parser Text
variableNameChunk = P.takeWhile1 (\c -> c /= '\\' && c /= '\n'
                                  && not (isSpace c)
                                  && c /= '#'  && c /= ':' && c /= '='
                                  && c /= '!' && c /= '?')

manyTill :: Parser a -> Parser b -> Parser ([a], b)
manyTill elemP end = scan []
  where
    scan xs = do { endVal <- end; return (reverse xs, endVal) ; } <|> do { nextVal <- elemP; scan (nextVal : xs) ; }

-- no :, =, # or whitespace
variableName :: Parser (Text, AssignOp)
variableName = do
  _ <- lineSpace
  (chunks, assign) <- manyTill (P.string "!" <|> P.string "?" <|> P.string "\\" <|> variableNameChunk) (lineSpace *> assignOp)
  return (T.concat chunks, assign)

commentOrMakeTextChunk :: String -> Bool -> Parser Text
commentOrMakeTextChunk escapedOk keepQuotedSpaceQuoted = let escapes = P.choice (map (\c -> P.char '\\' >> P.char c >> return (T.singleton c)) escapedOk) in
  lineSpaceCont P.many1' -- condense space around line continuation
  <|> P.takeWhile1 isLineSpace -- take space unchanged otherwhise
  <|> escapes -- one of the chars, which are ok if quoted
  <|> if keepQuotedSpaceQuoted
      then fmap (T.cons '\\' . T.singleton) (P.char '\\' >> P.satisfy isSpace) -- quoted space needs to preserve the quote -- testcase "\\  \\\n " must become "\\  " and not "\\ "
      else empty
  <|> P.string "\\" -- a quote itself is ok if it doesn't mask some magic char.
  <|> P.takeWhile1 (\c -> notElem c escapedOk && c /= '\\' && c /= '\n' && not (isSpace c))

comment :: Parser Text
comment = do
  _ <- P.char '#'
  rest <-  P.many' $ commentOrMakeTextChunk "" False
  return $ T.concat rest

standardMakeTextChunk :: String -> Parser Text
standardMakeTextChunk escapedOk = commentOrMakeTextChunk escapedOk True

variableAssignment :: Parser Entry
variableAssignment = do
  (name, assign) <- variableName
  _ <- lineSpace
  rest <- P.many' $ standardMakeTextChunk "#"
  let vartext = T.concat rest
  commentT <- P.option "" comment
  _ <- P.char '\n'
  return $ VariableAssignment (VariableName name) assign (VariableValue vartext) (Comment commentT)

simpleRule :: Parser Entry
simpleRule = do
  _ <- lineSpace
  tchunk       <- P.many' (standardMakeTextChunk ":#")
  _            <- P.char ':'
  dchunk       <- P.many' (standardMakeTextChunk "|#")
  odchunk      <- P.option [""] (P.char '|' >> P.many' (standardMakeTextChunk "#"))
  commentStuff <- P.option "" comment
  _ <- P.char '\n'
  recipeLines <- P.many' (P.many' ((lineSpace >> comment >> P.char '\n') <|> (lineSpace >> P.char '\n')) -- ignore whitespace lines, or comment only lines. Those are lost for now.
                                   >> recipeLine)
  return $ SimpleRule (Target . T.concat $ tchunk) Dependencies{ normal= Normal (T.concat dchunk), orderOnly = OrderOnly (T.concat odchunk) } recipeLines (Comment commentStuff)

-- rule "target stuff : depstuff | odepstuff # commentstuff"
-- targetstuff can only contain escaped : and only escaped #  everything else is normal make (line cont and stuff)
-- depstuff can contain escaped | and escaped # everything else is normal
-- odepstuff can contain escaped # everything else is normal
