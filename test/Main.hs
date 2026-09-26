-- | Единый набор тестов nspeller: unit, golden, property-based
-- и тесты веб-приложения Muzlovar.
module Main (main) where

import DtoTests (dtoTests)
import BilingualTests (bilingualTests)
import GoldenTests (goldenTests)
import Properties (propertyTests)
import RoundTripTests (roundTripTests)
import RegistryTests (registryTests)
import SchemaTests (schemaTests)
import ServerTests (serverTests)
import StoreTests (storeTests)
import Test.Tasty (defaultMain, testGroup)
import UnitTests (unitTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "nspeller"
      [ unitTests
      , bilingualTests
      , propertyTests
      , goldenTests
      , dtoTests
      , roundTripTests
      , registryTests
      , schemaTests
      , storeTests
      , serverTests
      ]
