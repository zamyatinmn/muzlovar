{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Property-based тесты.
--
-- Некоторые гарантии обеспечены конструкцией типов и потому
-- проверяются через наблюдаемые соседние свойства (подробнее — в
-- README, раздел «Типобезопасность»):
--
-- * несовместимые «поле/оператор» невозможно построить в 'ValidCond'
--   (GADT-индексация) — соседнее свойство сверяет порождённый
--   оператор и тип поля с таблицей совместимости документации;
-- * положительный 'vpLimit' гарантируется валидацией — свойство
--   проверяет, что модель NSP никогда не получит не-positive лимит.
module Properties (propertyTests) where

import Data.Aeson (Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import Generators
import Nspeller.Ast
import Nspeller.Navidrome
import Test.Tasty
import Test.Tasty.QuickCheck

-- | Сгенерированный JSON всегда декодируется aeson и содержит
-- корневую группу @all@ или @any@.
prop_jsonDecodable :: Property
prop_jsonDecodable = forAll genPlaylist $ \p ->
  let bytes = encodeNsp (toNsp p)
   in case eitherDecode bytes of
        Left err -> counterexample ("JSON не декодируется: " <> err <> "; " <> show bytes) False
        Right (v :: Value) -> case v of
          Object o -> property (KM.member "all" o || KM.member "any" o)
          _ -> counterexample ("ожидался объект, получено: " <> show v) False

-- | Валидированный AST не порождает несовместимых комбинаций
-- «поле/оператор»: оператор условия и тип её поля согласованы с
-- таблицей совместимости из документации Navidrome.
prop_operatorCompatible :: Property
prop_operatorCompatible = forAll genCond $ \c ->
  let op = condOperator c
      vt = condFieldVT c
   in counterexample
        ("оператор " <> show (navOpName op) <> " с типом поля " <> show vt)
        (navOpAllows op vt)

-- | 'nspLimit' в сгенерированной модели всегда положительный
-- (или не задан).
prop_limitPositive :: Property
prop_limitPositive = forAll genPlaylist $ \p ->
  case nspLimit (toNsp p) of
    Nothing -> property True
    Just n -> counterexample ("limit = " <> show n) (n > 0)

-- | Pretty-print каноничен: байты JSON не меняются после
-- декодирования и повторного кодирования тем же кодером. Вместе с
-- сортировкой ключей (`confCompare = compare`) это гарантирует
-- детерминированность сериализации между запусками; байтовая
-- стабильность между запусками дополнительно закреплена golden-тестами.
prop_prettyCanonical :: Property
prop_prettyCanonical = forAll genPlaylist $ \p ->
  let bytes = encodeNsp (toNsp p)
   in case eitherDecode bytes of
        Left err -> counterexample ("JSON не декодируется: " <> err) False
        Right (v :: Value) ->
          counterexample
            ("исходные байты: " <> show bytes <> "\nпосле roundtrip: " <> show (encodeNspValue v))
            (encodeNspValue v == bytes)

-- | Каждое поле сортировки известно схеме: сортировка устроена по
-- признаку 'sortFieldByName', и UI не содержит собственного списка.
prop_sortFieldsKnown :: Property
prop_sortFieldsKnown = forAll genPlaylist $ \p ->
  case vpSort p of
    Just (SortBy items) ->
      property $
        all (\(SortItem f _) -> sortFieldByName (sortFieldName f) == Just f) items
    _ -> property True

propertyTests :: TestTree
propertyTests =
  testGroup
    "Properties"
    [ testProperty "JSON декодируется aeson" prop_jsonDecodable
    , testProperty "оператор совместим с типом поля" prop_operatorCompatible
    , testProperty "limit в модели всегда положительный" prop_limitPositive
    , testProperty "pretty-print каноничен и детерминирован" prop_prettyCanonical
    , testProperty "поля сортировки известны схеме" prop_sortFieldsKnown
    ]
