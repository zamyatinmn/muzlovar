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
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
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
  | NGreater Text NspValue
  | NLess Text NspValue
  | NContains Text Text
  | NNotContains Text Text
  | NStartsWith Text Text
  | NEndsWith Text Text
  | NInRange Text NspValue NspValue
  | NInTheLast Text Integer
  | NNotInTheLast Text Integer
  | NBefore Text Day
  | NAfter Text Day
  | NIsMissing Text
  | NIsPresent Text
  | NAll [NspCondition]
  | NAny [NspCondition]
  | NInPlaylist PlaylistRef
  -- ^ Членство в подборке: @inPlaylist@ со ссылкой id/path.
  | NNotInPlaylist PlaylistRef
  -- ^ Отсутствие в подборке: @notInPlaylist@ со ссылкой id/path.
  deriving (Eq, Show)

-- | Значение операнда в JSON.
data NspValue
  = NspText Text
  | NspNumber Scientific
  | NspBool Bool
  | NspDate Day
  -- ^ Абсолютная дата: в JSON — строка @ГГГГ-ММ-ДД@ ('formatDay').
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- JSON
------------------------------------------------------------------------------

instance Aeson.ToJSON NspValue where
  toJSON = \case
    NspText t -> Aeson.toJSON t
    NspNumber n -> Aeson.toJSON n
    NspBool b -> Aeson.toJSON b
    NspDate d -> Aeson.toJSON (formatDay d)

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
    NInRange f lo hi -> object ["inTheRange" .= object [fromText f .= ([lo, hi] :: [NspValue])]]
    NInTheLast f days -> object ["inTheLast" .= object [fromText f .= days]]
    NNotInTheLast f days -> object ["notInTheLast" .= object [fromText f .= days]]
    NBefore f d -> object ["before" .= object [fromText f .= formatDay d]]
    NAfter f d -> object ["after" .= object [fromText f .= formatDay d]]
    NIsMissing f -> object ["isMissing" .= object [fromText f .= True]]
    NIsPresent f -> object ["isPresent" .= object [fromText f .= True]]
    NAll cs -> object ["all" .= cs]
    NAny cs -> object ["any" .= cs]
    NInPlaylist ref -> playlistCondJson "inPlaylist" ref
    NNotInPlaylist ref -> playlistCondJson "notInPlaylist" ref

-- | Сериализация условия-членства в подборке:
-- <@"inPlaylist": {"id": "..."}@> или <@"inPlaylist": {"path": "..."}@>
-- (и так же для @notInPlaylist@).
playlistCondJson :: Text -> PlaylistRef -> Value
playlistCondJson opName (PlaylistRef kind value) =
  object
    [ fromText opName
        .= object [fromText (playlistRefKindId kind) .= value]
    ]

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
toNspGroup (ValidGroup kind items) =
  -- Слияние пар «>= a» + «<= b» выполняется только для «все»:
  -- в «любое» условия лежат в разных ветвях и склеивать их нельзя.
  NspRoot kind (map toItem items')
  where
    items' = case kind of
      All -> mergeInclusivePairs items
      Any -> items

    toItem (VIC c) = toNspCond c
    toItem (VIG g) = case toNspGroup g of
      NspRoot All cs -> NAll cs
      NspRoot Any cs -> NAny cs

-- | Сторона включительной границы одного поля: поле и значения
-- нижней (@>=@) и верхней (@<=@) сторон ('Nothing' — сторона в этом
-- условии не задана). Числовые и датовые поля различаются типом
-- индекса 'FieldRef', поэтому набор объединён в один ADT.
data Side
  = NumSide (FieldRef Scientific) (Maybe Scientific) (Maybe Scientific)
  | DaySide (FieldRef Day) (Maybe Day) (Maybe Day)

-- | Чистая пара включительных границ одного поля (@>= a@ и @<= b@,
-- @a ≤ b@) внутри группы «все» заменяется одним условием
-- диапазона — оно компилируется в нативный @inTheRange@ Navidrome.
-- Порядок следования пары не важен; условия других видов и поля не
-- затрагиваются.
mergeInclusivePairs :: [ValidItem] -> [ValidItem]
mergeInclusivePairs = go
  where
    go [] = []
    go (x : xs) = case sideOf x of
      Nothing -> x : go xs
      Just s -> case extractJoined s xs of
        Nothing -> x : go xs
        Just (rest, c) -> VIC c : go rest

    -- Односторонняя граница: см. 'Side'.
    sideOf (VIC (VNumber f NGe a)) = Just (NumSide f (Just a) Nothing)
    sideOf (VIC (VNumber f NLe b)) = Just (NumSide f Nothing (Just b))
    sideOf (VIC (VDate f DGe a)) = Just (DaySide f (Just a) Nothing)
    sideOf (VIC (VDate f DLe b)) = Just (DaySide f Nothing (Just b))
    sideOf _ = Nothing

    -- Первое условие среди оставшихся, которое склеивается с
    -- стороной в упорядоченную пару; непарные элементы и
    -- невыполнимые пары (@lo > hi@) остаются нетронутыми.
    extractJoined s = search []
      where
        search _ [] = Nothing
        search acc (y : ys) = case sideOf y of
          Just t -> case joinSides s t of
            Just c -> Just (reverse acc ++ ys, c)
            Nothing -> search (y : acc) ys
          Nothing -> search (y : acc) ys

-- | Две стороны одного поля → условие-диапазон; 'Nothing' — поля
-- различаются либо нижняя граница больше верхней.
joinSides :: Side -> Side -> Maybe ValidCond
joinSides s t = case (s, t) of
  (NumSide f g1 l1, NumSide g g2 l2)
    | f == g -> do
        (lo, hi) <- bounds g1 l1 g2 l2
        if lo <= hi then Just (VBetween f lo hi) else Nothing
  (DaySide f g1 l1, DaySide g g2 l2)
    | f == g -> do
        (lo, hi) <- bounds g1 l1 g2 l2
        if lo <= hi then Just (VDateRange f lo hi) else Nothing
  _ -> Nothing
  where
    -- Одна из сторон — «>=» (нижняя), другая — «<=» (верхняя).
    bounds g1 l1 g2 l2 = case (g1, l2) of
      (Just lo, Just hi) -> Just (lo, hi)
      _ -> case (g2, l1) of
        (Just lo, Just hi) -> Just (lo, hi)
        _ -> Nothing

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
    NGt -> NGreater (fieldName f) (NspNumber n)
    NLt -> NLess (fieldName f) (NspNumber n)
    -- В НСП нет >= и <=: разворачиваем в «больше ИЛИ равно»
    -- (gt OR is) и «меньше ИЛИ равно» (lt OR is). Односторонняя
    -- граница останется этим выражением; пару в группе «все»
    -- предварительно склеивает 'mergeInclusivePairs'.
    NGe -> NAny [NGreater (fieldName f) (NspNumber n), NIs (fieldName f) (NspNumber n)]
    NLe -> NAny [NLess (fieldName f) (NspNumber n), NIs (fieldName f) (NspNumber n)]
  VBetween f lo hi -> NInRange (fieldName f) (NspNumber lo) (NspNumber hi)
  VBool f b -> NIs (fieldName f) (NspBool b)
  VRelative f rel days -> case rel of
    InTheLast -> NInTheLast (fieldName f) days
    NotInTheLast -> NNotInTheLast (fieldName f) days
  VDate f op d -> case op of
    DEq -> NIs (fieldName f) (NspDate d)
    DNe -> NIsNot (fieldName f) (NspDate d)
    -- Тот же приём «больше/меньше ИЛИ равно», что и у чисел
    -- (см. 'VNumber'): >= / <= выражаются через gt / lt OR is.
    DGt -> NGreater (fieldName f) (NspDate d)
    DGe -> NAny [NGreater (fieldName f) (NspDate d), NIs (fieldName f) (NspDate d)]
    DLt -> NLess (fieldName f) (NspDate d)
    DLe -> NAny [NLess (fieldName f) (NspDate d), NIs (fieldName f) (NspDate d)]
    DBefore -> NBefore (fieldName f) d
    DAfter -> NAfter (fieldName f) d
  VDateRange f lo hi -> NInRange (fieldName f) (NspDate lo) (NspDate hi)
  VPresence (SomeField f) Absent -> NIsMissing (fieldName f)
  VPresence (SomeField f) Present -> NIsPresent (fieldName f)
  VPlaylist InPlaylist ref -> NInPlaylist ref
  VPlaylist NotInPlaylist ref -> NNotInPlaylist ref

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
  | OpNavBefore
  | OpNavAfter
  | OpNavIsMissing
  | OpNavIsPresent
  | OpNavInPlaylist
  -- ^ @inPlaylist@ — членство в подборке по ссылке.
  | OpNavNotInPlaylist
  -- ^ @notInPlaylist@ — отсутствие в подборке по ссылке.
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
  OpNavBefore -> "before"
  OpNavAfter -> "after"
  OpNavIsMissing -> "isMissing"
  OpNavIsPresent -> "isPresent"
  OpNavInPlaylist -> "inPlaylist"
  OpNavNotInPlaylist -> "notInPlaylist"

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
    -- >= и <= выражаются через gt/lt (само разворачивание —
    -- в 'toNspCond'); таблица совместимости оператор/тип не меняется.
    NGt -> OpNavGt
    NGe -> OpNavGt
    NLt -> OpNavLt
    NLe -> OpNavLt
  VBetween{} -> OpNavInRange
  VBool{} -> OpNavIs
  VRelative _ rel _ -> case rel of
    InTheLast -> OpNavInTheLast
    NotInTheLast -> OpNavNotInTheLast
  VDate _ op _ -> case op of
    DEq -> OpNavIs
    DNe -> OpNavIsNot
    -- >= и <= выражаются через gt/lt (само разворачивание —
    -- в 'toNspCond'); таблица совместимости оператор/тип не меняется.
    DGt -> OpNavGt
    DGe -> OpNavGt
    DLt -> OpNavLt
    DLe -> OpNavLt
    DBefore -> OpNavBefore
    DAfter -> OpNavAfter
  VDateRange{} -> OpNavInRange
  VPresence _ p -> case p of
    Absent -> OpNavIsMissing
    Present -> OpNavIsPresent
  VPlaylist m _ -> case m of
    InPlaylist -> OpNavInPlaylist
    NotInPlaylist -> OpNavNotInPlaylist

-- | Категория значения поля условия.
condFieldVT :: ValidCond -> ValueType
condFieldVT = \case
  VText{} -> TextType
  VNumber{} -> NumberType
  VBetween{} -> NumberType
  VBool{} -> BoolType
  VRelative{} -> DateType
  VDate{} -> DateType
  VDateRange{} -> DateType
  VPresence (SomeField f) _ -> fieldValueType f
  VPlaylist{} -> PlaylistRefType

-- | Совместимость «оператор — тип поля» строго по документации
-- Navidrome. Свойство @navOpAllows (condOperator c) (condFieldVT c)@
-- проверяется для всех сгенерированных условий в property-тестах.
navOpAllows :: NavOperator -> ValueType -> Bool
navOpAllows op vt = case op of
  OpNavIs -> vt `elem` [TextType, NumberType, BoolType, DateType]
  OpNavIsNot -> vt `elem` [TextType, NumberType, BoolType, DateType]
  OpNavGt -> vt `elem` [NumberType, DateType]
  OpNavLt -> vt `elem` [NumberType, DateType]
  OpNavContains -> vt == TextType
  OpNavNotContains -> vt == TextType
  OpNavStartsWith -> vt == TextType
  OpNavEndsWith -> vt == TextType
  -- Числовые поля (@rating@, @year@ …) и датовые (@lastplayed@,
  -- @dateadded@): @inTheRange@ работает с обоими типами операнда.
  OpNavInRange -> vt `elem` [NumberType, DateType]
  OpNavInTheLast -> vt == DateType
  OpNavNotInTheLast -> vt == DateType
  OpNavBefore -> vt == DateType
  OpNavAfter -> vt == DateType
  -- Проверка наличия документирована для теговых/текстовых полей
  -- и числовых полей ReplayGain.
  OpNavIsMissing -> vt `elem` [TextType, NumberType]
  OpNavIsPresent -> vt `elem` [TextType, NumberType]
  -- Членство в подборке принимает только ссылку на подборку.
  OpNavInPlaylist -> vt == PlaylistRefType
  OpNavNotInPlaylist -> vt == PlaylistRefType
