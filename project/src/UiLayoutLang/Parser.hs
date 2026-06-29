{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module UiLayoutLang.Parser
  ( ParseError(..)
  , SourcePos(..)
  , parseProgram
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Text.Read (readMaybe)
import UiLayoutLang.Types
    ( Direction(..), Size(..), Layout(..), Program(..) )
import qualified Control.Monad

data SourcePos = SourcePos
  { sourceLine   :: Int
  , sourceColumn :: Int
  } deriving (Eq)

instance Show SourcePos where
  show :: SourcePos -> String
  show (SourcePos line column) =
    "line " ++ show line ++ ", column " ++ show column

data ParseError = ParseError SourcePos String
  deriving (Eq)

instance Show ParseError where
  show :: ParseError -> String
  show (ParseError pos msg) = show pos ++ ": " ++ msg

data Token = Token TokenKind SourcePos
  deriving (Eq, Show)

data TokenKind
  = TkIdent String
  | TkString String
  | TkNumber String
  | TkLBrace
  | TkRBrace
  | TkColon
  | TkComma
  | TkPercent
  deriving (Eq, Show)

newtype Parser a = Parser { runParser :: [Token] -> Either ParseError (a, [Token]) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = Parser $ \tokens -> do
    (x, rest) <- runParser parser tokens
    return (f x, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \tokens -> Right (x, tokens)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  parserF <*> parserX = Parser $ \tokens -> do
    (f, rest) <- runParser parserF tokens
    (x, rest') <- runParser parserX rest
    return (f x, rest')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  parser >>= f = Parser $ \tokens -> do
    (x, rest) <- runParser parser tokens
    runParser (f x) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \case
      [] -> Left (ParseError (SourcePos 1 1) "unexpected end of input")
      Token _ pos : _ -> Left (ParseError pos "unexpected token")

  (<|>) :: Parser a -> Parser a -> Parser a
  left <|> right = Parser $ \tokens ->
    (\case
      Right result -> Right result
      Left _       -> runParser right tokens) (runParser left tokens)

parseProgram :: String -> Either ParseError Program
parseProgram input = do
  tokens <- tokenize input
  (program, rest) <- runParser programParser tokens
  (\case
    [] -> Right program
    Token _ pos : _ -> Left (ParseError pos "unexpected tokens after program")) rest

programParser :: Parser Program
programParser = do
  _ <- expectIdent "window"
  title <- stringLiteral
  width <- positiveInt "window width"
  _ <- expectIdent "x"
  height <- positiveInt "window height"
  expect TkLBrace "'{'"
  root <- layoutParser
  expect TkRBrace "'}'"
  return Program
    { programTitle = title
    , programWidth = width
    , programHeight = height
    , programRoot = root
    }

layoutParser :: Parser Layout
layoutParser = do
  token <- takeToken
  direction <- layoutDirectionFromToken token
  expect TkLBrace "'{'"
  (layout, childFound) <- layoutBody direction emptyLayout
  expect TkRBrace "'}'"
  (\case
    (Leaf, True) -> failAt (tokenPos token) "box cannot contain child layouts; use row or col"
    _ -> return layout) (direction, childFound)

layoutDirectionFromToken :: Token -> Parser Direction
layoutDirectionFromToken token =
  byKind (tokenKind token)
  where
    byKind = \case
      TkIdent "row" -> return Row
      TkIdent "col" -> return Col
      TkIdent "box" -> return Leaf
      _ -> failAt (tokenPos token) "expected row, col, or box"

layoutBody :: Direction -> Layout -> Parser (Layout, Bool)
layoutBody direction layout = go layout False False False False
  where
    go current childFound hasWidth hasHeight hasColor = do
      next <- peekToken
      (\case
        Just TkRBrace ->
          return (current { layoutDirection = direction }, childFound)
        Just (TkIdent "width") -> do
          updated <- property "width" current hasWidth
          go updated childFound True hasHeight hasColor
        Just (TkIdent "height") -> do
          updated <- property "height" current hasHeight
          go updated childFound hasWidth True hasColor
        Just (TkIdent "color") -> do
          updated <- property "color" current hasColor
          go updated childFound hasWidth hasHeight True
        Just (TkIdent "row") -> do
          updated <- child current
          go updated True hasWidth hasHeight hasColor
        Just (TkIdent "col") -> do
          updated <- child current
          go updated True hasWidth hasHeight hasColor
        Just (TkIdent "box") -> do
          updated <- child current
          go updated True hasWidth hasHeight hasColor
        Just _ -> do
          token <- takeToken
          failAt (tokenPos token) "expected a property, row, col, box, or '}'"
        Nothing -> failAt (SourcePos 1 1) "unexpected end of input") (fmap tokenKind next)

    child current = do
      childLayout <- layoutParser
      optionalComma
      return current { layoutChildren = layoutChildren current ++ [childLayout] }

property :: String -> Layout -> Bool -> Parser Layout
property name layout isDuplicate = do
  pos <- expectIdent name
  expect TkColon "':'"
  updated <- propertyValue pos name layout isDuplicate
  optionalComma
  return updated

propertyValue :: SourcePos -> String -> Layout -> Bool -> Parser Layout
propertyValue pos name layout isDuplicate =
  byName name
  where
    byName = \case
      "width" -> do
        rejectDuplicate pos name isDuplicate
        size <- sizeParser
        return layout { layoutWidth = size }
      "height" -> do
        rejectDuplicate pos name isDuplicate
        size <- sizeParser
        return layout { layoutHeight = size }
      "color" -> do
        rejectDuplicate pos name isDuplicate
        color <- colorParser
        return layout { layoutColor = Just color }
      _ -> failAt pos "unknown property"

sizeParser :: Parser Size
sizeParser = do
  token <- takeToken
  (\case
    TkIdent "auto" -> return Auto
    TkNumber raw -> numberSize (tokenPos token) raw
    _ -> failAt (tokenPos token) "expected a size") (tokenKind token)

numberSize :: SourcePos -> String -> Parser Size
numberSize pos raw =
  (\case
    Nothing -> failAt pos ("invalid number " ++ raw)
    Just n
      | n < 0 -> failAt pos "size cannot be negative"
      | otherwise -> do
          next <- peekToken
          (\case
            Just TkPercent -> do
              _ <- takeToken
              if n <= 100
                then return (Pct (n / 100))
                else failAt pos "percentage size cannot be greater than 100"
            Just (TkIdent "px") -> do
              _ <- takeToken
              wholePixels pos raw
            _ -> wholePixels pos raw) (fmap tokenKind next)
    ) (readMaybe raw :: Maybe Double)

wholePixels :: SourcePos -> String -> Parser Size
wholePixels pos raw =
  (\case
    Just n -> return (Px n)
    Nothing -> failAt pos "pixel size must be a whole number") (readMaybe raw :: Maybe Int)

colorParser :: Parser String
colorParser = do
  token <- takeToken
  (\case
    TkIdent value -> return value
    TkString value -> return value
    _ -> failAt (tokenPos token) "expected a color name or string") (tokenKind token)

emptyLayout :: Layout
emptyLayout = Layout
  { layoutDirection = Leaf
  , layoutWidth = Auto
  , layoutHeight = Auto
  , layoutColor = Nothing
  , layoutChildren = []
  }

rejectDuplicate :: SourcePos -> String -> Bool -> Parser ()
rejectDuplicate pos name isDuplicate =
  Control.Monad.when isDuplicate
  $ failAt pos ("duplicate property " ++ name)

expectIdent :: String -> Parser SourcePos
expectIdent expected = do
  token <- takeToken
  (\case
    TkIdent actual
      | actual == expected -> return (tokenPos token)
    _ -> failAt (tokenPos token) ("expected " ++ expected)) (tokenKind token)

positiveInt :: String -> Parser Int
positiveInt label = do
  token <- takeToken
  (\case
    TkNumber raw ->
      (\case
        Just n
          | n > 0 -> return n
        _ -> failAt (tokenPos token) ("expected positive integer for " ++ label)) (readMaybe raw :: Maybe Int)
    _ -> failAt (tokenPos token) ("expected positive integer for " ++ label)) (tokenKind token)

stringLiteral :: Parser String
stringLiteral = do
  token <- takeToken
  (\case
    TkString value -> return value
    _ -> failAt (tokenPos token) "expected string literal") (tokenKind token)

expect :: TokenKind -> String -> Parser ()
expect expected label = do
  token <- takeToken
  if tokenKind token == expected
    then return ()
    else failAt (tokenPos token) ("expected " ++ label)

optionalComma :: Parser ()
optionalComma = do
  next <- peekToken
  (\case
    Just TkComma -> (Control.Monad.void takeToken)
    _ -> return ()) (fmap tokenKind next)

peekToken :: Parser (Maybe Token)
peekToken = Parser $ \tokens ->
  let result = (\case
        [] -> Nothing
        token : _ -> Just token) tokens
  in Right (result, tokens)

takeToken :: Parser Token
takeToken = Parser $ \case
    [] -> Left (ParseError (SourcePos 1 1) "unexpected end of input")
    token : rest -> Right (token, rest)

failAt :: SourcePos -> String -> Parser a
failAt pos msg = Parser $ \_ -> Left (ParseError pos msg)

tokenKind :: Token -> TokenKind
tokenKind (Token kind _) = kind

tokenPos :: Token -> SourcePos
tokenPos (Token _ pos) = pos

tokenize :: String -> Either ParseError [Token]
tokenize = tokenizeAt (SourcePos 1 1)

headSymbol :: TokenKind -> Char
headSymbol TkLBrace = '{'
headSymbol TkRBrace = '}'
headSymbol TkColon = ':'
headSymbol TkComma = ','
headSymbol TkPercent = '%'
headSymbol _ = ' '

identToken :: SourcePos -> String -> Either ParseError [Token]
identToken pos chars =
  let (name, rest) = span isIdentChar chars
      pos' = advanceMany pos name
  in do
    more <- tokenizeFrom pos' rest
    return (Token (TkIdent name) pos : more)

numberToken :: SourcePos -> String -> Either ParseError [Token]
numberToken pos chars =
  let (digits, rest) = span isDigit chars
  in (\case
    '.' : d : more
      | isDigit d ->
          let (afterDot, rest') = span isDigit (d:more)
              raw = digits ++ "." ++ afterDot
          in finish raw rest'
    _ -> finish digits rest) rest
  where
    finish raw rest = do
      more <- tokenizeFrom (advanceMany pos raw) rest
      return (Token (TkNumber raw) pos : more)

stringToken :: SourcePos -> String -> Either ParseError [Token]
stringToken pos = go [] (advance pos '"')
  where
    go _ current [] = Left (ParseError current "unterminated string literal")
    go acc current ('"':rest) = do
      more <- tokenizeFrom (advance current '"') rest
      return (Token (TkString (reverse acc)) pos : more)
    go acc current ('\\':'"':rest) = go ('"':acc) (advanceMany current "\\\"") rest
    go acc current ('\\':'\\':rest) = go ('\\':acc) (advanceMany current "\\\\") rest
    go acc current ('\\':'n':rest) = go ('\n':acc) (advanceMany current "\\n") rest
    go acc current (c:rest) = go (c:acc) (advance current c) rest

tokenizeFrom :: SourcePos -> String -> Either ParseError [Token]
tokenizeFrom = tokenizeAt

tokenizeAt :: SourcePos -> String -> Either ParseError [Token]
tokenizeAt = go
  where
    go _ [] = Right []
    go pos chars@(c:cs)
      | isSpace c = go (advance pos c) cs
      | c == '#' = go (skipLine pos chars) (dropLine chars)
      | take 2 chars == "//" = go (skipLine pos chars) (dropLine chars)
      | take 2 chars == "/*" = blockComment pos (drop 2 chars) >>= uncurry go
      | c == '{' = tokenWith TkLBrace pos cs
      | c == '}' = tokenWith TkRBrace pos cs
      | c == ':' = tokenWith TkColon pos cs
      | c == ',' = tokenWith TkComma pos cs
      | c == '%' = tokenWith TkPercent pos cs
      | c == '"' = stringToken pos cs
      | isDigit c = numberToken pos chars
      | isAlpha c || c == '_' = identToken pos chars
      | otherwise = Left (ParseError pos ("unexpected character " ++ [c]))

    tokenWith kind pos rest = do
      more <- go (advance pos (headSymbol kind)) rest
      return (Token kind pos : more)

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '-'

advance :: SourcePos -> Char -> SourcePos
advance (SourcePos line _) '\n' = SourcePos (line + 1) 1
advance (SourcePos line column) _ = SourcePos line (column + 1)

advanceMany :: SourcePos -> String -> SourcePos
advanceMany = foldl advance

dropLine :: String -> String
dropLine [] = []
dropLine ('\n':rest) = rest
dropLine (_:rest) = dropLine rest

skipLine :: SourcePos -> String -> SourcePos
skipLine pos [] = pos
skipLine pos ('\n':_) = advance pos '\n'
skipLine pos (c:rest) = skipLine (advance pos c) rest

blockComment :: SourcePos -> String -> Either ParseError (SourcePos, String)
blockComment pos = go (advanceMany pos "/*")
  where
    go current [] = Left (ParseError current "unterminated block comment")
    go current ('*':'/':rest) = Right (advanceMany current "*/", rest)
    go current (c:rest) = go (advance current c) rest
