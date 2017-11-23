{-# LANGUAGE OverloadedStrings #-}
module Data.Makefile.Parse.Internal where

import Data.Makefile

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser, (<?>))

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
lineContinuation = P.string "\\\n" <?> "line continuation"

recipeLineContinuation :: Parser Text
recipeLineContinuation = P.string "\\\n\t" >> return "\\\n" <?> "recipe line continuation"

recipeLineStuff :: Parser RecipeLine
recipeLineStuff = do
  rest <- P.many' (recipeLineContinuation <|> lineContinuation <|> P.string "\\" <|> P.takeWhile1 (\c -> c /= '\\' && c /= '\n'))
  return $ RecipeLine $ T.concat rest

recipeLine :: Parser RecipeLine
recipeLine = P.char '\t' *> recipeLineStuff <* P.char '\n'

isLineSpace :: Char -> Bool
isLineSpace c = c /= '\n' && isSpace c

lineSpaceCont :: (Parser Text -> Parser [Text]) -> Parser Text
lineSpaceCont f = (P.takeWhile isLineSpace >> f (lineContinuation >> P.takeWhile isLineSpace) >> return " ") <?> "line space or line continuation"

lineSpace :: Parser Text
lineSpace = lineSpaceCont P.many'


assignOp :: Parser AssignOp
assignOp = ((P.string "::=" >> return PosixSimple)
       <|> (P.string ":=" >> return Simple)
       <|> (P.string "?=" >> return Conditional)
       <|> (P.string "!=" >> return Shell)
       <|> (P.string "+=" >> return Append)
       <|> (P.string "=" >> return Recursive)) <?> "assign operation"

variableNameChunk :: Parser Text
variableNameChunk = P.takeWhile1 (\c -> c /= '\\' && c /= '\n'
                                  && not (isSpace c)
                                  && c /= '#'  && c /= ':' && c /= '='
                                  && c /= '!' && c /= '?') <?> "variable name chunk"

manyTill :: Parser a -> Parser b -> Parser ([a], b)
manyTill elemP end = scan [] <?> "many till"
  where
    scan xs = do { endVal <- end; return (reverse xs, endVal) ; } <|> do { nextVal <- elemP; scan (nextVal : xs) ; }

-- no :, =, # or whitespace
variableName :: Parser (Text, AssignOp)
variableName = (do
  _ <- lineSpace
  (chunks, assign) <- manyTill (P.string "!" <|> P.string "?" <|> P.string "\\" <|> variableNameChunk) (lineSpace *> assignOp)
  return (T.concat chunks, assign)) <?> "variable name and assignment"

commentOrMakeTextChunk :: String -> Bool -> Parser Text
commentOrMakeTextChunk escapedOk keepQuotedSpaceQuoted = let escapes = P.choice (map (\c -> P.char '\\' >> P.char c >> return (T.singleton c)) escapedOk) in
  (lineSpaceCont P.many1' -- condense space around line continuation
  <|> P.takeWhile1 isLineSpace -- take space unchanged otherwhise
  <|> escapes -- one of the chars, which are ok if quoted
  <|> (if keepQuotedSpaceQuoted
      then fmap (T.cons '\\' . T.singleton) (P.char '\\' >> P.satisfy isSpace) -- quoted space needs to preserve the quote -- testcase "\\  \\\n " must become "\\  " and not "\\ "
      else empty)
  <|> P.string "\\" -- a quote itself is ok if it doesn't mask some magic char.
  <|> P.takeWhile1 (\c -> notElem c escapedOk && c /= '\\' && c /= '\n' && not (isSpace c))) <?> "basic make text chunk"

comment :: Parser Comment
comment = (do
  _ <- P.char '#'
  rest <-  T.concat <$> P.many' (commentOrMakeTextChunk "" False)
  return $ Comment rest) <?> "comment"

standardMakeTextChunk :: String -> Parser Text
standardMakeTextChunk escapedOk = commentOrMakeTextChunk escapedOk True

variableAssignment :: Parser Entry
variableAssignment = do
  (name, assign) <- variableName
  _ <- lineSpace
  rest <- P.many' $ standardMakeTextChunk "#"
  let vartext = T.concat rest
  commentT <- P.option "" comment
  _ <- P.char '\n' <?> "variable assignment newline"
  return $ VariableAssignment (VariableName name) assign (VariableValue vartext) commentT

inlineRecipeChunk :: Parser RecipeLine
inlineRecipeChunk = P.char ';' *> recipeLineStuff

ignoreWhitespaceOrCommentsLines :: Parser ()
ignoreWhitespaceOrCommentsLines = (P.many' $ (lineSpace >> comment >> P.char '\n') <|> -- comment line
                                             (lineSpace >> P.char '\n')) >> return ()   -- whitespace or empty line

ruleRest :: Parser (Dependencies, [RecipeLine], Comment)
ruleRest = do
  _ <- lineSpace
  dchunk       <- T.concat <$> P.many' (standardMakeTextChunk "|#;")
  odchunk      <- T.concat <$> P.option [""] (P.char '|' >> P.many' (standardMakeTextChunk "#;"))
  maybeCommentOrRecipeline <- P.option Nothing $ ((Just . Left) <$> comment) <|>
                                                 ((Just . Right) <$> inlineRecipeChunk)
  _ <- P.char '\n'
  recipeLines <- P.many' (ignoreWhitespaceOrCommentsLines *> recipeLine)
  let (r, c) = case maybeCommentOrRecipeline of
                 Nothing         -> (recipeLines, Comment "")
                 Just (Left co)  -> (recipeLines, co)
                 Just (Right re) -> (re : recipeLines, Comment "")
  return (Dependencies{normal = Normal dchunk, orderOnly = OrderOnly odchunk}, r, c)

simpleRule :: Parser Entry
simpleRule = do
  _ <- lineSpace
  tchunk    <- T.concat <$> P.many' (standardMakeTextChunk ":%#")
  _         <- P.char ':'
  (d, r, c) <- ruleRest
  return $ SimpleRule (Target tchunk) d r c

patternRule :: Parser Entry
patternRule = do
  _         <- lineSpace
  tchunk1   <- T.concat <$> P.many' (standardMakeTextChunk ":%#")
  _         <- P.char '%'
  tchunk2   <- T.concat <$> P.many' (standardMakeTextChunk ":%#")
  _         <- P.char ':'
  (d, r, c) <- ruleRest
  return $ PatternRule (Target $ T.concat [tchunk1, "%", tchunk2]) d r c

staticPatternRule :: Parser Entry
staticPatternRule = do
  _ <- lineSpace
  ttchunk   <- T.concat <$> P.many' (standardMakeTextChunk ":#")
  _         <- P.char ':'
  tchunk1   <- T.concat <$> P.many' (standardMakeTextChunk ":%#")
  _         <- P.char '%'
  tchunk2   <- T.concat <$> P.many' (standardMakeTextChunk ":%#")
  _         <- P.char ':'
  (d, r, c) <- ruleRest
  return $ StaticPatternRule (Target ttchunk) (Target $ T.concat [tchunk1, "%", tchunk2]) d r c

entry :: Parser Entry
entry =  do
  _ <- P.many' (lineSpace >> P.char '\n')
  staticPatternRule <|>
    patternRule <|>
    simpleRule <|>
    variableAssignment <|>
    (CommentLine <$> (lineSpace *> comment <* P.char '\n'))

makefile :: Parser Makefile
makefile = Makefile <$> P.many' entry
