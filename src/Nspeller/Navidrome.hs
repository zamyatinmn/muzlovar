{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Модель smart playlist Navidrome (@.nsp@) и её JSON-сериализация.
--
-- Поле и операторы соответствуют официальной документации:
-- <https://www.navidrome.org/docs/usage/features/smart-playlists/>.
module Nspeller.Navidrome
  ( -- * Модель NSP
    NspPlaylist (..)
  , NspRoot (..)
  , NspCondition (..)
  , NspValue (..)

    -- * Построение из валидированного AST
  , toNsp

    -- * Сериализация
  , encodeNsp
  , encodeNspValue

    -- * Операторы Navidrome (для документации и тестов)
  , NavOperator (..)
  , navOpName
  , condOperator
  , condFieldVT
  , navOpAllows
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.Aeson.Key (fromText)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Nspeller.Ast

------------------------------------------------------------------------------
-- Модель
------------------------------------------------------------------------------

-- | Готовая smart playlist: один JSON-объект.
data NspPlaylist = NspPlaylist
  { nspName :: Text
  -- ^ @name@ — обязательно.
  , nspComment :: Maybe Text
  -- ^ @comment@ — только если задано описание.
  , nspPublic :: Maybe Bool
  -- ^ @public@ — только для публичных подборок.
  , nspRoot :: NspRoot
  -- ^ Единственная корневая группа @all@ или @any@.
  , nspSort :: Maybe Text
  -- ^ @sort@ — @random@ или список полей через запятую.
  , nspLimit :: Maybe Integer
  -- ^ @limit@ — только положительные значения.
  }
  deriving (Eq, Show)

-- | Корневая группа условий (ровно одна, как требует Navidrome).
data NspRoot = NspRoot
  { nrKind :: GroupKind
  , nrConditions :: [NspCondition]
  }
  deriving (Eq, Show)

-- | Условие или вложенная группа в терминах Navidrome.
data NspCondition
  = NIs Text NspValue
  | NIsNot Text NspValue
  | NGreater Text Integer
  | NLess Text Integer
  | NContains Text Text
  | NNotContains Text Text
  | NStartsWith Text Text
  | NEndsWith Text Text
  | NInRange Text Integer Integer
  | NInTheLast Text Integer
  | NNotInTheLast Text Integer
  | NIsMissing Text
  | NIsPresent Text
  | NAll [NspCondition]
  | NAny [NspCondition]
  deriving (Eq, Show)

-- | Значение операнда в JSON.
data NspValue
  = NspText Text
  | NspNumber Integer
  | NspBool Bool
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- JSON
------------------------------------------------------------------------------

instance Aeson.ToJSON NspValue where
  toJSON = \case
    NspText t -> Aeson.toJSON t
    NspNumber n -> Aeson.toJSON n
    NspBool b -> Aeson.toJSON b

instance Aeson.ToJSON NspCondition where
  toJSON = \case
    NIs f v -> object ["is" .= object [fromText f .= v]]
    NIsNot f v -> object ["isNot" .= object [fromText f .= v]]
    NGreater f n -> object ["gt" .= object [fromText f .= n]]
    NLess f n -> object ["lt" .= object [fromText f .= n]]
    NContains f t -> object ["contains" .= object [fromText f .= t]]
    NNotContains f t -> object ["notContains" .= object [fromText f .= t]]
    NStartsWith f t -> object ["startsWith" .= object [fromText f .= t]]
    NEndsWith f t -> object ["endsWith" .= object [fromText f .= t]]
    NInRange f lo hi -> object ["inTheRange" .= object [fromText f .= ([lo, hi] :: [Integer])]]
    NInTheLast f days -> object ["inTheLast" .= object [fromText f .= days]]
    NNotInTheLast f days -> object ["notInTheLast" .= object [fromText f .= days]]
    NIsMissing f -> object ["isMissing" .= object [fromText f .= True]]
    NIsPresent f -> object ["isPresent" .= object [fromText f .= True]]
    NAll cs -> object ["all" .= cs]
    NAny cs -> object ["any" .= cs]

instance Aeson.ToJSON NspPlaylist where
  toJSON p = object $ concat
    [ ["name" .= nspName p]
    , maybe [] (\c -> ["comment" .= c]) (nspComment p)
    , maybe [] (\b -> ["public" .= b]) (nspPublic p)
    , [kindKey .= nrConditions root]
    , maybe [] (\s -> ["sort" .= s]) (nspSort p)
    , maybe [] (\n -> ["limit" .= n]) (nspLimit p)
    ]
    where
      root = nspRoot p
      kindKey = case nrKind root of
        All -> "all"
        Any -> "any"

------------------------------------------------------------------------------
-- Построение
------------------------------------------------------------------------------

-- | Перевод валидированного AST в модель Navidrome.
toNsp :: ValidPlaylist -> NspPlaylist
toNsp vp = NspPlaylist
  { nspName = vpName vp
  , nspComment = vpDescription vp
  , nspPublic = if vpPublic vp then Just True else Nothing
  , nspRoot = toNspGroup (vpRoot vp)
  , nspSort = sortText <$> vpSort vp
  , nspLimit = vpLimit vp
  }

toNspGroup :: ValidGroup -> NspRoot
toNspGroup (ValidGroup kind items) = NspRoot kind (map toItem items)
  where
    toItem (VIC c) = toNspCond c
    toItem (VIG g) = case toNspGroup g of
      NspRoot All cs -> NAll cs
      NspRoot Any cs -> NAny cs

sortText :: SortMode -> Text
sortText SortRandomMode = "random"
sortText (SortBy items) =
  T.intercalate "," [dirPrefix d <> sortFieldName f | SortItem f d <- items]
  where
    dirPrefix SortAsc = ""
    dirPrefix SortDesc = "-"

toNspCond :: ValidCond -> NspCondition
toNspCond = \case
  VText f op t -> case op of
    TEq -> NIs (fieldName f) (NspText t)
    TNe -> NIsNot (fieldName f) (NspText t)
    TContains -> NContains (fieldName f) t
    TNotContains -> NNotContains (fieldName f) t
    TStartsWith -> NStartsWith (fieldName f) t
    TEndsWith -> NEndsWith (fieldName f) t
  VNumber f op n -> case op of
    NEq -> NIs (fieldName f) (NspNumber n)
    NNe -> NIsNot (fieldName f) (NspNumber n)
    NGt -> NGreater (fieldName f) n
    NLt -> NLess (fieldName f) n
  VBetween f lo hi -> NInRange (fieldName f) lo hi
  VBool f b -> NIs (fieldName f) (NspBool b)
  VRelative f rel days -> case rel of
    InTheLast -> NInTheLast (fieldName f) days
    NotInTheLast -> NNotInTheLast (fieldName f) days
  VPresence p Absent -> NIsMissing (presenceFieldName p)
  VPresence p Present -> NIsPresent (presenceFieldName p)

------------------------------------------------------------------------------
-- Сериализация
------------------------------------------------------------------------------

prettyConfig :: Config
prettyConfig =
  defConfig
    { confIndent = Spaces 2
    -- Ключи сортируются по алфавиту: сериализация детерминирована
    -- и не зависит от порядка обхода карты ключей.
    , confCompare = compare
    }

-- | Pretty-print с отступом в два пробела и завершающим переводом
-- строки. Детерминирован для одного и того же значения.
encodeNsp :: NspPlaylist -> LBS.ByteString
encodeNsp = encodeNspValue . Aeson.toJSON

-- | Pretty-print произвольного 'Value' тем же способом
-- (используется в property-тестах для проверки каноничности).
encodeNspValue :: Value -> LBS.ByteString
encodeNspValue v = encodePretty' prettyConfig v <> "\n"

------------------------------------------------------------------------------
-- Операторы Navidrome
------------------------------------------------------------------------------

-- | Операторы из документации Navidrome (для таблиц совместимости).
data NavOperator
  = OpNavIs
  | OpNavIsNot
  | OpNavGt
  | OpNavLt
  | OpNavContains
  | OpNavNotContains
  | OpNavStartsWith
  | OpNavEndsWith
  | OpNavInRange
  | OpNavInTheLast
  | OpNavNotInTheLast
  | OpNavIsMissing
  | OpNavIsPresent
  deriving (Eq, Show, Enum, Bounded)

-- | Имя оператора в NSP.
navOpName :: NavOperator -> Text
navOpName = \case
  OpNavIs -> "is"
  OpNavIsNot -> "isNot"
  OpNavGt -> "gt"
  OpNavLt -> "lt"
  OpNavContains -> "contains"
  OpNavNotContains -> "notContains"
  OpNavStartsWith -> "startsWith"
  OpNavEndsWith -> "endsWith"
  OpNavInRange -> "inTheRange"
  OpNavInTheLast -> "inTheLast"
  OpNavNotInTheLast -> "notInTheLast"
  OpNavIsMissing -> "isMissing"
  OpNavIsPresent -> "isPresent"

-- | Оператор Navidrome, порождаемый условием валидированного AST.
condOperator :: ValidCond -> NavOperator
condOperator = \case
  VText _ op _ -> case op of
    TEq -> OpNavIs
    TNe -> OpNavIsNot
    TContains -> OpNavContains
    TNotContains -> OpNavNotContains
    TStartsWith -> OpNavStartsWith
    TEndsWith -> OpNavEndsWith
  VNumber _ op _ -> case op of
    NEq -> OpNavIs
    NNe -> OpNavIsNot
    NGt -> OpNavGt
    NLt -> OpNavLt
  VBetween{} -> OpNavInRange
  VBool{} -> OpNavIs
  VRelative _ rel _ -> case rel of
    InTheLast -> OpNavInTheLast
    NotInTheLast -> OpNavNotInTheLast
  VPresence _ p -> case p of
    Absent -> OpNavIsMissing
    Present -> OpNavIsPresent

-- | Категория значения поля условия.
condFieldVT :: ValidCond -> ValueType
condFieldVT = \case
  VText{} -> TextType
  VNumber{} -> NumberType
  VBetween{} -> NumberType
  VBool{} -> BoolType
  VRelative{} -> DateType
  VPresence p _ -> presenceValueType p

-- | Совместимость «оператор — тип поля» строго по документации
-- Navidrome. Свойство @navOpAllows (condOperator c) (condFieldVT c)@
-- проверяется для всех сгенерированных условий в property-тестах.
navOpAllows :: NavOperator -> ValueType -> Bool
navOpAllows op vt = case op of
  OpNavIs -> vt `elem` [TextType, NumberType, BoolType]
  OpNavIsNot -> vt `elem` [TextType, NumberType, BoolType]
  OpNavGt -> vt == NumberType
  OpNavLt -> vt == NumberType
  OpNavContains -> vt == TextType
  OpNavNotContains -> vt == TextType
  OpNavStartsWith -> vt == TextType
  OpNavEndsWith -> vt == TextType
  -- Мы порождаем inTheRange только для числовых полей
  -- (датовые диапазоны в DSL отсутствуют).
  OpNavInRange -> vt == NumberType
  OpNavInTheLast -> vt == DateType
  OpNavNotInTheLast -> vt == DateType
  -- Проверка наличия документирована для теговых/текстовых полей
  -- и числовых полей ReplayGain.
  OpNavIsMissing -> vt `elem` [TextType, NumberType]
  OpNavIsPresent -> vt `elem` [TextType, NumberType]
