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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Nspeller.Ast
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

genTextField :: Gen (Field Text)
genTextField = elements [Title, Album, Genre, ExplicitStatus]

genNumberField :: Gen (Field Integer)
genNumberField = elements [Year, Rating, PlayCount, RGTrackGain]

genDateField :: Gen (Field Day)
genDateField = elements [LastPlayed, DateAdded]

genCond :: Gen ValidCond
genCond =
  oneof
    [ VText <$> genTextField <*> elements [minBound .. maxBound] <*> genText
    , VNumber <$> genNumberField <*> elements [minBound .. maxBound] <*> arbitrary
    , VBetween <$> genNumberField <*> arbitrary <*> arbitrary
    , VBool <$> elements [Loved, HasCoverArt] <*> arbitrary
    , VRelative <$> genDateField <*> elements [minBound .. maxBound] <*> (getPositive <$> arbitrary)
    , VPresence <$> elements [minBound .. maxBound] <*> elements [minBound .. maxBound]
    ]

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
      vectorOf n (SortItem <$> elements [minBound .. maxBound] <*> elements [minBound .. maxBound])

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
