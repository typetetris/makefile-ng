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
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Maybe (fromJust)

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

stopAtClosing :: (Char -> Bool) -> (Char -> Bool) -> Char -> Char -> Int -> Parser (Text, Int)
stopAtClosing accOutside accNested start stop danglingOpen = go
  where
    go = P.runScanner danglingOpen (\stillOpen c -> case c of
                                 _ | c == start -> Just (stillOpen + 1)
                                   | c == stop && stillOpen > 0 -> Just (stillOpen - 1)
                                   | c == stop && stillOpen == 0 -> Nothing
                                   | stillOpen == 0 && accOutside c -> Just stillOpen
                                   | stillOpen > 0 && accNested c -> Just stillOpen
                                   | otherwise -> Nothing)

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

quoteNotLineContinuation :: Parser Text
quoteNotLineContinuation = do
  _ <- P.char '\\'
  c <- P.peekChar
  case c of
    (Just '\n') -> fail "line continuation"
    _           -> return $ T.singleton '\\'

variableAssignment :: Parser Entry
variableAssignment = do
  (name, assign) <- variableName
  _ <- lineSpace
  rest <- P.many' $ standardMakeTextChunk "#"
  let vartext = T.concat rest
  commentT <- P.option "" comment
  _ <- P.char '\n' <?> "variable assignment newline"
  return $ VariableAssignment (VariableName name) assign (VariableValue vartext) commentT

multiLineVariableName :: Parser (Text, AssignOp)
multiLineVariableName = lineSpace *> ((\c -> (c, Recursive)) <$> (P.string "!" <|> P.string "?" <|> quoteNotLineContinuation <|> variableNameChunk))

multiLineVariableDefinitionLine :: Parser (VariableName, AssignOp, Comment)
multiLineVariableDefinitionLine  = do
  _ <- P.string "define"
  _ <- lineSpace
  (name, assign) <- variableName <|> multiLineVariableName
  _ <- lineSpace
  commentT <- P.option (Comment "") comment
  _<- P.char '\n'
  return (VariableName name, assign, commentT)

multiLineVariableBody :: Parser [VariableValue]
multiLineVariableBody = manyTill ((VariableValue . T.concat <$>
                                   P.many' (commentOrMakeTextChunk "" False)) <* P.char '\n')
                        (P.string "endef\n") >>= \(b, _) -> return b

multiLineVariableAssignment :: Parser Entry
multiLineVariableAssignment = do
  (n, a, c) <- multiLineVariableDefinitionLine
  b <- multiLineVariableBody
  return $ MultilineVariableAssignment n a b c

inlineRecipeChunk :: Parser RecipeLine
inlineRecipeChunk = P.char ';' *> recipeLineStuff

ignoreWhitespaceOrCommentsLines :: Parser ()
ignoreWhitespaceOrCommentsLines = void (P.many' $ (lineSpace >> comment >> P.char '\n') <|> -- comment line
                                             (lineSpace >> P.char '\n'))   -- whitespace or empty line

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

targetPattern :: Parser Target
targetPattern = do
  tchunk1   <- T.concat <$> P.many' (standardMakeTextChunk ":%#")
  _         <- P.char '%'
  tchunk2   <- T.concat <$> P.many' (standardMakeTextChunk ":%#")
  _         <- P.char ':'
  return $ Target $ T.concat [tchunk1, "%", tchunk2]

patternRule :: Parser Entry
patternRule = do
  _         <- lineSpace
  t         <- targetPattern
  (d, r, c) <- ruleRest
  return $ PatternRule t d r c

staticPatternRule :: Parser Entry
staticPatternRule = do
  _ <- lineSpace
  ttchunk   <- T.concat <$> P.many' (standardMakeTextChunk ":#")
  _         <- P.char ':'
  t         <- targetPattern
  (d, r, c) <- ruleRest
  return $ StaticPatternRule (Target ttchunk) t d r c

entry :: Parser Entry
entry =  do
  _ <- P.many' (lineSpace >> P.char '\n')
  variableAssignment <|>
    staticPatternRule <|>
    patternRule <|>
    simpleRule <|>
    (CommentLine <$> (lineSpace *> comment <* P.char '\n')) <|>
    multiLineVariableAssignment

makefile :: Parser Makefile
makefile = Makefile <$> P.many' entry

unevaluatedText :: Parser UnevaluatedText
unevaluatedText = UnevaluatedText <$> P.many' (utchunk $ const True)

utchunk :: (Char -> Bool) -> Parser Chunk
utchunk acc = utplain acc <|>
          utvariableReferenceDelims <|>
          utfunctionCall <|>
          utvariableReferenceSingleChar <|>
          (Plain <$> P.string "$$")

utplain :: (Char -> Bool) -> Parser Chunk
utplain acc = Plain <$> P.takeWhile1 (\c -> c /= '$' && acc c)

unevaluatedText' :: Char -> Char -> (Char -> Bool) -> (Char -> Bool) -> Parser UnevaluatedText
unevaluatedText' start stop accOutside accNested = UnevaluatedText . concat <$> P.many' (unevaluatedText'' start stop accOutside accNested 0 [])

unevaluatedText'' :: Char -> Char -> (Char -> Bool) -> (Char -> Bool) -> Int -> [Chunk] -> Parser [Chunk]
unevaluatedText'' start stop accOutside accNested stillOpen cs = do
  (c, stillOpen') <- utchunk' start stop accOutside accNested stillOpen
  if stillOpen' == 0
    then return $ reverse $ c : cs
    else unevaluatedText'' start stop accOutside accNested stillOpen' (c:cs)

utchunk' :: Char -> Char -> (Char -> Bool) -> (Char -> Bool) -> Int -> Parser (Chunk, Int)
utchunk' start stop accOutside accNested stillOpen =
  utplain' start stop (\c -> accOutside c && c /= '$') (\c -> accNested c && c /= '$') stillOpen <|>
  (utvariableReferenceDelims >>= (\c -> return (c, stillOpen))) <|>
  (utfunctionCall >>= (\c -> return (c, stillOpen))) <|>
  (utvariableReferenceSingleChar >>= (\c -> return (c,stillOpen))) <|>
  (P.string "$$" >> return (Plain "$$", stillOpen))

utplain' :: Char -> Char -> (Char -> Bool) -> (Char -> Bool) -> Int -> Parser (Chunk, Int)
utplain' start stop accOutside accNested stillOpen = do
  (t, stillOpen') <- stopAtClosing accOutside accNested start stop stillOpen
  if T.length t == 0
    then fail "at least one valid character expected"
    else return (Plain t, stillOpen')

utvariableReferenceSingleChar :: Parser Chunk
utvariableReferenceSingleChar = do
  _ <- P.char '$'
  c <- P.satisfy (\c -> c /= ':' && c /= '#' && c /= '=' && not (isSpace c))
  return $ VariableReference $ UnevaluatedText [Plain $ T.singleton c]

closingChar :: Char -> Maybe Char
closingChar '(' = Just ')'
closingChar '{' = Just '}'
closingChar _ = Nothing

utvariableReferenceDelims :: Parser Chunk
utvariableReferenceDelims = do
  _ <- P.char '$'
  start <- P.satisfy (\c -> c == '(' || c == '{')
  let stop = fromJust $ closingChar start
  let charTest = \c -> c `notElem` (":#=" :: String) && not (isSpace c)
  name <- unevaluatedText' start stop charTest charTest
  _ <- P.char stop
  return $ VariableReference name

utfunctionCall :: Parser Chunk
utfunctionCall = do
  _ <- P.char '$'
  start <- P.satisfy (\c -> c == '(' || c == '{')
  fName <- P.takeWhile1 (\c -> c /= ' ' && c /= '\t')
  _ <- P.takeWhile1 (\c -> c == ' ' || c == '\t')
  let stop = fromJust $ closingChar start
  args <- P.sepBy (unevaluatedText' start stop (/=',') (const True)) (P.char ',')
  _ <- P.char stop <?> "Function call closing delimeter"
  return $ FunctionCall fName args
