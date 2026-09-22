-- | Единый набор тестов nspeller: unit, golden и property-based.
module Main (main) where

import GoldenTests (goldenTests)
import Properties (propertyTests)
import Test.Tasty (defaultMain, testGroup)
import UnitTests (unitTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "nspeller"
      [ unitTests
      , propertyTests
      , goldenTests
      ]
