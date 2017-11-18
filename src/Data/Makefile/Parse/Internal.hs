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

comment :: Parser Text
comment = do
  _ <- P.char '#'
  rest <-  P.many' $ lineSpaceCont P.many1' <|> P.takeWhile1 isLineSpace <|> P.takeWhile1 (not . isSpace)
  return $ T.concat rest

variableAssignment :: Parser Entry
variableAssignment = do
  (name, assign) <- variableName
  _ <- lineSpace
  rest <- P.many' $ P.string "\\#" <|> lineSpaceCont P.many1' <|> P.string "\\" <|> P.takeWhile1 isLineSpace <|> P.takeWhile1 (\c -> c /= '#' && c /= '\n' && c /= '\\' && not (isSpace c))
  let vartext = T.concat rest
  commentT <- P.option "" comment
  _ <- P.char '\n'
  return $ VariableAssignment (VariableName name) assign (VariableValue vartext) (Comment commentT)

simpleRule :: Parser Entry
simpleRule = do
  _ <- lineSpace
  (tchunk, _) <- manyTill (lineSpaceCont P.many1' -- all space gets reduced to one space, but we need a parser that can fail.
                           <|> (P.takeWhile1 isLineSpace >> P.string " ") -- see above
                           <|> (P.string "\\:" >> return ":") -- quoted colon is ok
                           <|> (P.string "\\#" >> return "#") -- quoted hash sign is ok
                           <|> do { _ <- P.char '\\'; c <- P.satisfy isSpace; return $ T.cons '\\' (T.singleton c) ; } -- quoted space is ok and we need to keep the backslash
                                                                                                                       -- that breaking into words later works.
                           <|> P.string "\\" -- backspace is ok, but only if it doesn't escape some magic character
                           <|> P.takeWhile1 (\c -> c /= '\\' && c/= '\n' && c /= ':' && c /= '#') -- meat of the targets
                          ) (lineSpace *> P.string ":")

  dchunk <- P.many' (lineSpaceCont P.many1' -- condense space
                          <|> (P.takeWhile1 isLineSpace >> P.string " ") -- condense space
                          <|> (P.string "\\|" >> return "|") -- quoted stuff ok
                          <|> (P.string "\\#" >> return "#") -- quoted stuff ok
                          <|> P.takeWhile1 (\c -> c /= '|' && c /= '\n' && c /= '\\' && c /= '#') -- meet of deps ok
                         ) -- should end at unquoted \n or unquoted #

  odchunk <- P.option [""] (P.char '|' >> P.many' (lineSpaceCont P.many1' -- condense space
                            <|> (P.takeWhile1 isLineSpace >> P.string " ") -- condense space
                            <|> (P.string "\\#" >> return "#") -- quoted stuff ok
                            <|> P.takeWhile1 (\c -> c /= '\n' && c /= '\\' && c /= '#') -- meet of deps ok
                            )) -- should end at unquoted \n or unquoted #
  commentStuff <- P.option "" comment
  _ <- P.char '\n'
  recipeLines <- P.many' (P.many' ((lineSpace >> comment >> P.char '\n') <|> (lineSpace >> P.char '\n')) -- ignore whitespace lines, or comment only lines. Those are lost for now.
                                   >> recipeLine)
  return $ SimpleRule (Target . T.concat $ tchunk) Dependencies{ normal= Normal (T.concat dchunk), orderOnly = OrderOnly (T.concat odchunk) } recipeLines (Comment commentStuff)

-- rule "target stuff : depstuff | odepstuff # commentstuff"
-- targetstuff can only contain escaped : and only escaped #  everything else is normal make (line cont and stuff)
-- depstuff can contain escaped | and escaped # everything else is normal
-- odepstuff can contain escaped # everything else is normal
