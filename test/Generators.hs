{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Генераторы QuickCheck для валидированного AST и модели NSP.
--
-- Генерируются только корректные ('ValidPlaylist'): типы полей и
-- операторов согласованы уже на уровне конструкторов, а лимит
-- всегда положительный.
module Generators
  ( genPlaylist
  , genCond
  , genGroup
  , genText
  ) where

import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Nspeller.Ast
import Nspeller.Fields
  ( FieldKind (..)
  , SomeFieldSpec (..)
  , fieldSpecs
  , fieldsOfKind
  , spDslName
  , spPresence
  , spSortable
  , specHasCapability
  )
import Test.QuickCheck

-- | Небольшой текст (кириллица, латиница, цифры), без управляющих
-- символов и кавычек.
genText :: Gen Text
genText =
  T.pack
    <$> listOf
      ( elements
          ("абвгдежзиклмнопрстуфхцчшщэюяАБВГДЕЖЗИКЛМНОПРСТУФХЦЧШЩЭЮЯ"
             ++ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
             ++ "0123456789 -.,")
      )

-- | Поля текстового вида: выбор из реестра по виду
-- ('fieldsOfKind') — собственного списка полей у генератора нет.
genTextField :: Gen (FieldRef Text)
genTextField = elements (fieldsOfKind KindText)

genNumberField :: Gen (FieldRef Scientific)
genNumberField = elements (fieldsOfKind KindNumber)

-- | Дробное число: у QuickCheck нет 'Arbitrary' для 'Scientific'.
-- Диапазон небольшой, чтобы значения оставались читаемыми в
-- сообщениях и JSON.
genScientific :: Gen Scientific
genScientific = Sci.scientific <$> choose (-10000, 10000) <*> choose (-2, 2)

genDateField :: Gen (FieldRef Day)
genDateField = elements (fieldsOfKind KindDate)

-- | Абсолютная дата в разумном диапазоне. 'fromGregorian'
-- нормализует несуществующие дни (31 февраля → 1 марта), поэтому
-- 'formatDay' всегда печатает валидную @ГГГГ-ММ-ДД@.
genDay :: Gen Day
genDay = fromGregorian <$> choose (1950, 2050) <*> choose (1, 12) <*> choose (1, 31)

-- | Поля с проверкой наличия: выводятся из реестра 'fieldSpecs' по
-- признаку 'spPresence' среди статических полей — отдельного списка
-- полей в тестах нет.
presenceFields :: [SomeField]
presenceFields =
  [ SomeField (FieldRef spec)
  | SomeFieldSpec spec <- fieldSpecs
  , spPresence spec
  , specHasCapability CapStatic spec
  ]

-- | DSL-имена сортируемых полей: тот же вывод из реестра по
-- признаку 'spSortable'.
sortableNames :: [Text]
sortableNames = [spDslName spec | SomeFieldSpec spec <- fieldSpecs, spSortable spec]

genCond :: Gen ValidCond
genCond =
  oneof
    [ VText <$> genTextField <*> elements [minBound .. maxBound] <*> genText
    , VNumber <$> genNumberField <*> elements [minBound .. maxBound] <*> genScientific
    , VBetween <$> genNumberField <*> genScientific <*> genScientific
    , VBool <$> elements (fieldsOfKind KindBool) <*> arbitrary
    , VRelative <$> genDateField <*> elements [minBound .. maxBound] <*> (getPositive <$> arbitrary)
    -- Границы диапазона не упорядочиваются: round-trip проверяет
    -- обратимость рендера, а не валидацию ('prop_dtoRenderParse'
    -- не перепроверяет AST).
    , VDate <$> genDateField <*> elements [minBound .. maxBound] <*> genDay
    , VDateRange <$> genDateField <*> genDay <*> genDay
    , VPresence <$> elements presenceFields <*> elements [minBound .. maxBound]
    , VPlaylist <$> elements [minBound .. maxBound] <*> genPlaylistRef
    ]

-- | Ссылка на подборку: вид и непустое значение (пустая ссылка —
-- ошибка валидации, генератор выдаёт только валидированный AST).
genPlaylistRef :: Gen PlaylistRef
genPlaylistRef =
  PlaylistRef
    <$> elements [minBound .. maxBound]
    <*> genRefValue
  where
    genRefValue =
      T.pack
        <$> ( chooseInt (1, 8)
                >>= \n -> vectorOf n (elements "abcdef0123456789-./")
            )

genItems :: Int -> Gen [ValidItem]
genItems depth = do
  n <- chooseInt (1, 4)
  vectorOf n (genItem depth)

genItem :: Int -> Gen ValidItem
genItem depth
  | depth <= 0 = VIC <$> genCond
  | otherwise =
      frequency
        [ (3, VIC <$> genCond)
        , (2, VIG <$> genGroup depth)
        ]

genGroup :: Int -> Gen ValidGroup
genGroup depth = ValidGroup <$> elements [minBound .. maxBound] <*> genItems (depth - 1)

genSort :: Gen SortMode
genSort =
  oneof
    [ pure SortRandomMode
    , SortBy <$> genSortItems
    ]
  where
    genSortItems = do
      n <- chooseInt (1, 4)
      vectorOf n (SortItem <$> elements sortableNames <*> elements [minBound .. maxBound])

genPositive :: Gen Integer
genPositive = getPositive <$> arbitrary

-- | Валидированная подборка: лимит (если есть) всегда положительный.
genPlaylist :: Gen ValidPlaylist
genPlaylist =
  ValidPlaylist
    <$> genText
    <*> oneof [pure Nothing, Just <$> genText]
    <*> arbitrary
    <*> genGroup 3
    <*> oneof [pure Nothing, Just <$> genSort]
    <*> oneof [pure Nothing, Just <$> genPositive]
