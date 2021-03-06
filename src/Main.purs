module Main where

import Prelude
import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointFromChar, uncons)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)

-- | Represents a parsed value from JSON
data JsonValue
  = JsonNull
  | JsonBool Boolean

derive instance eqJsonValue :: Eq JsonValue

instance showJsonValue :: Show JsonValue where
  show JsonNull = "null"
  show (JsonBool true) = "true"
  show (JsonBool false) = "false"

-- | A Parser for an arbitrary type a
newtype Parser a
  = Parser (String -> Maybe { next :: String, p :: a })

instance functorParser :: Functor Parser where
  map :: forall a b. (a -> b) -> Parser a -> Parser b
  map f (Parser parser) =
    Parser
      $ \input -> do
          { next, p } <- parser input
          Just { next: next, p: f p }

instance applyParser :: Apply Parser where
  apply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  -- | Runs the first parser, pipes the output into the second parser, and
  -- | applies the first parser's function to the second parser's result.
  apply (Parser parserFirst) (Parser parserSecond) =
    Parser
      $ \input -> do
          { next: next, p: parsedFirst } <- parserFirst input
          { next: next', p: parsedSecond } <- parserSecond next
          Just { next: next', p: parsedFirst parsedSecond }

instance applicativeParser :: Applicative Parser where
  -- | Returns a parser that always succeeds, proxying its input, and parsing a
  pure :: forall a. a -> Parser a
  pure a = Parser \input -> Just { next: input, p: a }

instance altParser :: Alt Parser where
  alt :: forall a. Parser a -> Parser a -> Parser a
  alt (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance plusParser :: Plus Parser where
  empty :: forall a. Parser a
  empty = Parser $ const Nothing

instance alternativeParser :: Alternative Parser

runParser :: forall a. Parser a -> String -> Maybe { next :: String, p :: a }
runParser (Parser p) = p

-- | Parses a single character c
charP :: Char -> Parser Char
charP c = Parser f
  where
  f s = do
    { head: head, tail: tail } <- uncons s
    if head == (codePointFromChar c) then
      Just $ { next: tail, p: c }
    else
      Nothing

-- | Parses a given constant string s
stringP :: String -> Parser String
stringP s =
  let
    charArray = charP <$> toCharArray s
  in
    fromCharArray <$> sequence charArray

jsonNullParser :: Parser JsonValue
jsonNullParser = const JsonNull <$> stringP "null"

jsonBoolParser :: Parser JsonValue
jsonBoolParser =
  (const (JsonBool true) <$> stringP "true")
    <|> (const (JsonBool false) <$> stringP "false")

jsonValueParser :: Parser JsonValue
jsonValueParser = jsonNullParser <|> jsonBoolParser
