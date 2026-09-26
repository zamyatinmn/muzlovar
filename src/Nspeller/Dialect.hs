{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Surface spellings of the two .mix dialects. These are presentation
-- tokens only; field IDs, the validated AST and .nsp keys are shared.
module Nspeller.Dialect
  ( DslDialect (..)
  , DslKeyword (..)
  , dialectText
  , parseDialect
  , keyword
  ) where

import Data.Text (Text)

data DslDialect = Ru | En
  deriving (Eq, Show, Enum, Bounded)

dialectText :: DslDialect -> Text
dialectText Ru = "ru"
dialectText En = "en"

parseDialect :: Text -> Maybe DslDialect
parseDialect "ru" = Just Ru
parseDialect "en" = Just En
parseDialect _ = Nothing

data DslKeyword
  = KPlaylist | KDescription | KPublic | KWhere | KSort | KRandom | KLimit
  | KAll | KAny | KAscending | KDescending
  | KTrue | KFalse | KBetween | KAnd
  | KNotPlayed | KInPlaylist | KNotInPlaylist | KRefId | KRefFile
  | KNotWithin | KWithin | KDays | KBefore | KAfter
  | KNotContains | KStartsWith | KEndsWith | KContains
  | KMissing | KPresent
  deriving (Eq, Show, Enum, Bounded)

keyword :: DslDialect -> DslKeyword -> Text
keyword Ru = \case
  KPlaylist -> "подборка"
  KDescription -> "описание"
  KPublic -> "публичная"
  KWhere -> "где"
  KSort -> "порядок"
  KRandom -> "случайный"
  KLimit -> "лимит"
  KAll -> "все"
  KAny -> "любое"
  KAscending -> "возр"
  KDescending -> "убыв"
  KTrue -> "да"
  KFalse -> "нет"
  KBetween -> "между"
  KAnd -> "и"
  KNotPlayed -> "не звучало"
  KInPlaylist -> "в подборке"
  KNotInPlaylist -> "не в подборке"
  KRefId -> "id"
  KRefFile -> "файл"
  KNotWithin -> "не за"
  KWithin -> "за"
  KDays -> "дней"
  KBefore -> "до"
  KAfter -> "после"
  KNotContains -> "не содержит"
  KStartsWith -> "начинается с"
  KEndsWith -> "заканчивается на"
  KContains -> "содержит"
  KMissing -> "отсутствует"
  KPresent -> "присутствует"
keyword En = \case
  KPlaylist -> "playlist"
  KDescription -> "description"
  KPublic -> "public"
  KWhere -> "where"
  KSort -> "sort"
  KRandom -> "random"
  KLimit -> "limit"
  KAll -> "all"
  KAny -> "any"
  KAscending -> "ascending"
  KDescending -> "descending"
  KTrue -> "true"
  KFalse -> "false"
  KBetween -> "between"
  KAnd -> "and"
  KNotPlayed -> "not played within"
  KInPlaylist -> "in playlist"
  KNotInPlaylist -> "not in playlist"
  KRefId -> "id"
  KRefFile -> "file"
  KNotWithin -> "not within"
  KWithin -> "within"
  KDays -> "days"
  KBefore -> "before"
  KAfter -> "after"
  KNotContains -> "does not contain"
  KStartsWith -> "starts with"
  KEndsWith -> "ends with"
  KContains -> "contains"
  KMissing -> "is missing"
  KPresent -> "is present"
