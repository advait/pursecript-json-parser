module Test.Main where

import Main
import Prelude
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar, dropWhile)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "purescript-spec" do
          describe "charP" do
            it "parses when c is prefixed"
              $ quickCheck \(CharTest { c, suffix }) ->
                  let
                    testS = (singleton c) <> suffix
                  in
                    runParser (charP c) testS === Just { next: suffix, p: c }
            it "does not parse when c is not prefixed"
              $ quickCheck \(CharTest { c, suffix }) ->
                  let
                    testS = dropWhile ((==) (codePointFromChar c)) suffix
                  in
                    runParser (charP c) testS === Nothing

newtype CharTest
  = CharTest { c :: Char, suffix :: String }

instance charTestArb :: Arbitrary CharTest where
  arbitrary :: Gen CharTest
  arbitrary = do
    c <- arbitrary
    suffix <- arbitrary
    pure $ CharTest { c: c, suffix: suffix }
