module Test.Main where

import Main
import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (codePointFromChar, dropWhile, stripPrefix)
import Data.String.CodeUnits (singleton)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.QuickCheck ((===))
import Test.Spec (SpecT(..), describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "purescript-json-parser" do
          describe "charP" do
            it "parses when c is prefixed"
              $ quickCheck \(Tuple c suffix) ->
                  let
                    testS = (singleton c) <> suffix
                  in
                    runParser (charP c) testS === Just { next: suffix, p: c }
            it "does not parse when c is not prefixed"
              $ quickCheck \(Tuple c suffix) ->
                  let
                    testS = dropWhile ((==) (codePointFromChar c)) suffix
                  in
                    runParser (charP c) testS === Nothing
          describe "stringP" do
            it "parses when s is prefixed"
              $ quickCheck \(Tuple prefix suffix) ->
                  runParser (stringP prefix) (prefix <> suffix) === Just { next: suffix, p: prefix }
            it "does not parse when s is not prefixed"
              $ quickCheck \(Tuple prefix suffix) ->
                  let
                    testS :: String
                    testS = (fromMaybe suffix) $ stripPrefix (Pattern prefix) suffix
                  in
                    runParser (stringP prefix) testS
                      === if prefix == "" then
                          Just { next: testS, p: "" }
                        else
                          Nothing
          describe "jsonNullParser" $ quickCheckJsonValue JsonNull
          describe "jsonBoolParser" do
            quickCheckJsonValue $ JsonBool false
            quickCheckJsonValue $ JsonBool true

-- it "parses null successfully"
--   $ quickCheck \suffix ->
--       runParser (jsonNullParser) ("null" <> suffix) === Just { next: suffix, p: JsonNull }
-- it "does not parse when null is not prefixed"
--   $ quickCheck \suffix ->
--       runParser (jsonNullParser) (dropWhile ((/=) (codePointFromChar 'n')) suffix) === Nothing
quickCheckJsonValue :: forall a. Bind a => Monad a => JsonValue -> SpecT Aff Unit a Unit
quickCheckJsonValue v = do
  it ("parses " <> (show v) <> " successfully")
    $ quickCheck \suffix ->
        runParser (jsonValueParser) (show v <> suffix) === Just { next: suffix, p: v }
  it ("does not parse when " <> (show v) <> " is not prefix")
    $ quickCheck \suffix ->
        let
          testS = (fromMaybe suffix) $ stripPrefix (Pattern (show v)) suffix
        in
          runParser jsonValueParser testS === Nothing
