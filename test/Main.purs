module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Record.Format (format, parseURL)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)
import Type.Prelude (SProxy(..))

main :: Effect Unit
main = do
  let formatted = format (SProxy :: SProxy "Hi {name}! You are {number}") {name : "Bill", number : 16}
  assert $ formatted == "Hi Bill! You are 16"

  let (parseURL'
        -- inferred type:
        :: String -> Either String { name :: String, age :: String})
        = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
  let parsed = parseURL' "/hello/Bill/12"
  case parsed of
    Left e -> do
      log $ "didn't work: " <> e
      assert $ 1 == 2
    Right r -> do
      assert $ r.name == "Bill"
      assert $ r.age == "12"
