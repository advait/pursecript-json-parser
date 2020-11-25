module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointFromChar, uncons)

data JsonValue
  = JsonNull
  | JsonBool Boolean

newtype Parser a
  = Parser (String -> Maybe { next :: String, p :: a })

runParser :: forall a. Parser a -> String -> Maybe { next :: String, p :: a }
runParser (Parser p) = p

charP :: Char -> Parser Char
charP c = Parser f
  where
  f s = do
    { head: head, tail: tail } <- uncons s
    if head == (codePointFromChar c) then
      Just $ { next: tail, p: c }
    else
      Nothing
