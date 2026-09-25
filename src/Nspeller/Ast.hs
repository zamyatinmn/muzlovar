{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Алгебраические типы данных nspeller.
--
-- Модуль содержит три группы понятий:
--
-- * разобранный ('ParsedFile'), ещё не проверенный AST;
-- * валидированный ('ValidPlaylist'), типобезопасный AST;
-- * описание ошибок компиляции ('CompileError') и их форматирование.
--
-- Поля Navidrome представлены ссылками 'FieldRef', индексированными
-- типом значения поля. Это делает некорректные комбинации «поле/
-- оператор» непредставимыми в валидированном AST: конструктор 'VText'
-- принимает только 'FieldRef' 'Text', 'VNumber' — только 'FieldRef'
-- 'Scientific' и т. д.
--
-- Метаданные полей (реестр 'defaultRegistry', имена, категории,
-- операторы, признаки, возможности, ограничения) живут в
-- 'Nspeller.Fields'; оттуда же приходят и переэкспортируются здесь
-- типы полей, ссылок и сортировки — потребителям известен только
-- этот модуль.
module Nspeller.Ast
  ( -- * Позиции в исходнике
    Located (..)
  , errorAt
  , errorAtFileStart
  , fileError
  , posFromOffset
  , lineTextAt

    -- * Категории значений полей
  , ValueType (..)
  , valueTypeDesc
  , valueTypeExpect

    -- * Ссылки на подборки (членство в подборке)
  , PlaylistRefKind (..)
  , PlaylistRef (..)
  , PlaylistMembership (..)
  , playlistRefDslName
  , playlistRefKindDsl
  , playlistRefKindId

    -- * Поля Navidrome
  , FieldRef (..)
  , SomeField (..)
  , FieldCapability (..)
  , fieldHasCapability
  , someFieldHasCapability
  , fieldName
  , fieldValueType
  , fieldByName
  , fieldDslName
  , fieldPresence
  , EnumVariant (..)
  , fieldEnum
  , fieldIsIntegral
  , fieldMultivalue
  , formatNumber
  , isIntegralNumber
  , formatDay
  , parseDay

    -- * Разобранный (невалидированный) AST
  , ParsedFile (..)
  , Statement (..)
  , GroupKind (..)
  , LogicGroup (..)
  , CondItem (..)
  , RawCond (..)
  , RawOp (..)
  , rawOpDesc
  , NumConstraints (..)
  , fieldNumConstraints
  , RawValue (..)
  , rawValueText
  , PresenceOp (..)
  , presenceOpDesc
  , RawSort (..)
  , RawSortItem (..)
  , RawDir (..)

    -- * Валидированный AST
  , ValidPlaylist (..)
  , ValidGroup (..)
  , ValidItem (..)
  , ValidCond (..)
  , TextOp (..)
  , NumOp (..)
  , RelOp (..)
  , DateOp (..)
  , SortMode (..)
  , SortDir (..)
  , SortItem (..)
  , sortFieldName
  , sortFieldByName

    -- * Ошибки компиляции
  , CompileError (..)
  , renderCompileError
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Ratio (denominator)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorianValid, toGregorian)
import Nspeller.Fields
import Text.Read (readMaybe)

------------------------------------------------------------------------------
-- Позиции и размещённые элементы
------------------------------------------------------------------------------

-- | Элемент исходника с позицией его начала и конца
-- (смещения в символах, как их считает 'megaparsec' для потока 'Text').
data Located a = Located
  { locStart :: !Int
  , locEnd :: !Int
  , locValue :: a
  }
  deriving (Eq, Show, Functor)

-- | Вычисляет строку и столбец (оба с единицы) по смещению в исходнике.
posFromOffset :: Text -> Int -> (Int, Int)
posFromOffset src off0 =
  let cs = T.unpack (T.take (max 0 off0) src)
      newlines = length (filter (== '\n') cs)
      afterLastNL = case break (== '\n') (reverse cs) of
        (lineRev, _) -> length lineRev
   in (newlines + 1, afterLastNL + 1)

-- | Текст строки @n@ (с единицы) исходника; завершающий @\\r@ отбрасывается.
lineTextAt :: Text -> Int -> Text
lineTextAt src n =
  let ls = T.splitOn "\n" src
   in maybe "" (T.dropWhileEnd (== '\r')) (atMay ls (n - 1))
  where
    atMay xs i = if i < 0 || i >= length xs then Nothing else Just (xs !! i)

-- | Ошибка внутри фрагмента исходника: подчёркивание покрывает
-- текст от начала до конца размещённого элемента (но не длиннее строки).
errorAt :: FilePath -> Text -> Located a -> NonEmpty Text -> CompileError
errorAt fp src (Located s e _) msgs =
  let s' = max 0 (min s (T.length src))
      (ln, col) = posFromOffset src s'
      lt = lineTextAt src ln
      lineEnd = s' + T.length (T.takeWhile (/= '\n') (T.drop s' src))
      spanW = max 1 (min e lineEnd - s')
   in CompileError fp (Just (ln, col)) lt spanW msgs

-- | Ошибка, относящаяся к началу файла (отсутствующая секция):
-- позиция 1:1 с одиночным курсором.
errorAtFileStart :: FilePath -> Text -> NonEmpty Text -> CompileError
errorAtFileStart fp src msgs =
  CompileError fp (Just (1, 1)) (lineTextAt src 1) 1 msgs

-- | Ошибка без позиции в исходнике (файловая система, кодировка).
fileError :: FilePath -> Text -> CompileError
fileError fp msg = CompileError fp Nothing "" 0 (msg :| [])

------------------------------------------------------------------------------
-- Числа
------------------------------------------------------------------------------

-- | Каноническое текстовое представление числа для @.mix@ и
-- сообщений об ошибках: целые — без дробной части (@1980@), иначе —
-- без экспоненты и без хвостовых нулей (@-6.5@).
--
-- @show@ здесь нельзя использовать: он даёт @1980.0@ и ломает
-- round-trip «парсер ⇄ рендер».
formatNumber :: Scientific -> Text
formatNumber s
  | isIntegralNumber s = T.pack (Sci.formatScientific Sci.Fixed (Just 0) (Sci.normalize s))
  | otherwise = T.pack (Sci.formatScientific Sci.Fixed Nothing (Sci.normalize s))

-- | Целое ли значение несёт число (@1980.0@ — целое, @-6.5@ — нет).
isIntegralNumber :: Scientific -> Bool
isIntegralNumber n = denominator (toRational n) == 1

------------------------------------------------------------------------------
-- Даты
------------------------------------------------------------------------------

-- | Каноническое текстовое представление даты для @.mix@, NSP и
-- сообщений об ошибках: @ГГГГ-ММ-ДД@ с ведущими нулями
-- (@2024-01-05@). Обратная операция к 'parseDay'.
formatDay :: Day -> Text
formatDay day =
  let (y, m, d) = toGregorian day
   in T.intercalate "-" [pad 4 y, pad 2 m, pad 2 d]
  where
    pad :: Show a => Int -> a -> Text
    pad n v = T.justifyRight n '0' (T.pack (show v))

-- | Разбирает дату в формате @ГГГГ-ММ-ДД@ (ровно ASCII-цифры);
-- @2024-02-30@ и прочие несуществующие даты дают 'Nothing'.
--
-- Обратная операция к 'formatDay': @parseDay (formatDay d) == Just d@.
parseDay :: Text -> Maybe Day
parseDay t = do
  (yTxt, mTxt, dTxt) <- case T.splitOn "-" t of
    [ys, ms, ds] | allDigits 4 ys && allDigits 2 ms && allDigits 2 ds ->
      Just (ys, ms, ds)
    _ -> Nothing
  y <- readMaybe (T.unpack yTxt)
  m <- readMaybe (T.unpack mTxt)
  d <- readMaybe (T.unpack dTxt)
  fromGregorianValid y m d
  where
    allDigits n s = T.length s == n && T.all isAsciiDigit s
    isAsciiDigit c = c >= '0' && c <= '9'

------------------------------------------------------------------------------
-- Разобранный AST
------------------------------------------------------------------------------

-- | Результат разбора файла: список секций в порядке появления.
newtype ParsedFile = ParsedFile {unParsedFile :: [Located Statement]}
  deriving (Eq, Show)

-- | Верхнеуровневая секция файла.
data Statement
  = SName Text
  -- ^ @подборка "…"@
  | SDescription Text
  -- ^ @описание "…"@
  | SPublic
  -- ^ @публичная@
  | SWhere LogicGroup
  -- ^ @где все { … }@ / @где любое { … }@
  | SSort RawSort
  -- ^ @порядок случайный@ / @порядок { … }@
  | SLimit Integer
  -- ^ @лимит 100@
  deriving (Eq, Show)

-- | Логика объединения элементов группы.
data GroupKind = All | Any
  deriving (Eq, Show, Enum, Bounded)

-- | Логическая группа условий.
data LogicGroup = LogicGroup
  { lgKind :: GroupKind
  , lgItems :: [Located CondItem]
  }
  deriving (Eq, Show)

-- | Элемент внутри группы: условие или вложенная группа.
data CondItem
  = CICond RawCond
  | CIGroup LogicGroup
  deriving (Eq, Show)

-- | Условие в разобранном (сыром) виде: имена полей — строки,
-- проверка типов выполняется на этапе валидации.
data RawCond
  = RBare Text
  -- ^ Сахар: @любимое@ (равно @любимое = да@).
  | RBin Text RawOp RawValue
  -- ^ @поле оператор значение@
  | RBetween Text Scientific Scientific
  -- ^ @replaygain между -8 и -4@
  | RPresence Text PresenceOp
  -- ^ @replaygain отсутствует@
  | RRelative Text Integer
  -- ^ @добавлено за 30 дней@
  | RNotRelative Text Integer
  -- ^ @добавлено не за 30 дней@ — «не за N дней» для любого
  -- датового поля (для поля с возможностью 'CapNotPlayed'
  -- используется сахар 'RNotPlayed').
  | RNotPlayed Integer
  -- ^ @не звучало 90 дней@ — сахар без имени поля: цель берётся
  -- из реестра по возможности 'CapNotPlayed'.
  | RDateBetween Text Day Day
  -- ^ @добавлено между 2024-01-01 и 2024-12-31@ — абсолютный
  -- диапазон дат (для числовых полей используется 'RBetween').
  | RPlaylist PlaylistMembership PlaylistRef
  -- ^ @в подборке id "…"@ / @не в подборке файл "…"@ — членство
  -- трека в другой подборке (@inPlaylist@/@notInPlaylist@).
  deriving (Eq, Show)

-- | Бинарные операторы DSL.
data RawOp
  = OpEq
  | OpNe
  | OpGt
  | OpGe
  | OpLt
  | OpLe
  | OpContains
  | OpNotContains
  | OpStartsWith
  | OpEndsWith
  | OpBefore
  -- ^ @до 2024-06-01@ — только датовые поля.
  | OpAfter
  -- ^ @после 2024-06-01@ — только датовые поля.
  deriving (Eq, Show, Enum, Bounded)

-- | Написание оператора в DSL (для сообщений об ошибках).
rawOpDesc :: RawOp -> Text
rawOpDesc = \case
  OpEq -> "="
  OpNe -> "!="
  OpGt -> ">"
  OpGe -> ">="
  OpLt -> "<"
  OpLe -> "<="
  OpContains -> "содержит"
  OpNotContains -> "не содержит"
  OpStartsWith -> "начинается с"
  OpEndsWith -> "заканчивается на"
  OpBefore -> "до"
  OpAfter -> "после"

-- | Значение-операнд в условии.
data RawValue
  = RVText Text
  | RVNumber Scientific
  | RVBool Bool
  | RVDate Day
  -- ^ Абсолютная дата в формате @ГГГГ-ММ-ДД@ (в том числе
  -- без кавычек — грамматика даты однозначна).
  deriving (Eq, Show)

-- | Отображение значения в сообщениях об ошибках.
rawValueText :: RawValue -> Text
rawValueText = \case
  RVText t -> t
  RVNumber n -> formatNumber n
  RVBool True -> "да"
  RVBool False -> "нет"
  RVDate d -> formatDay d

-- | Проверка наличия значения поля.
data PresenceOp = Absent | Present
  deriving (Eq, Show, Enum, Bounded)

-- | Написание оператора наличия в DSL.
presenceOpDesc :: PresenceOp -> Text
presenceOpDesc = \case
  Absent -> "отсутствует"
  Present -> "присутствует"

-- | Секция сортировки.
data RawSort
  = SortRandom
  | SortSpec [Located RawSortItem]
  deriving (Eq, Show)

-- | Элемент сортировки: поле и направление.
data RawSortItem = RawSortItem Text RawDir
  deriving (Eq, Show)

-- | Направление сортировки в DSL.
data RawDir = Ascending | Descending
  deriving (Eq, Show, Enum, Bounded)

------------------------------------------------------------------------------
-- Валидированный AST
------------------------------------------------------------------------------

-- | Проверенная подборка: все имена полей разрешены, все операторы
-- совместимы с типами полей, все значения корректны.
data ValidPlaylist = ValidPlaylist
  { vpName :: Text
  , vpDescription :: Maybe Text
  , vpPublic :: Bool
  , vpRoot :: ValidGroup
  , vpSort :: Maybe SortMode
  , vpLimit :: Maybe Integer
  }
  deriving (Eq, Show)

-- | Валидированная логическая группа.
data ValidGroup = ValidGroup GroupKind [ValidItem]
  deriving (Eq, Show)

-- | Элемент валидированной группы.
data ValidItem
  = VIC ValidCond
  | VIG ValidGroup
  deriving (Eq, Show)

-- | Валидированное условие. Конструкторы фиксируют допустимые
-- сочетания «тип поля — оператор — тип значения»: поле — ссылка
-- 'FieldRef' на запись реестра, тип которой задан её видом.
data ValidCond
  = VText (FieldRef Text) TextOp Text
  -- ^ Текстовое поле и текстовый операнд.
  | VNumber (FieldRef Scientific) NumOp Scientific
  -- ^ Числовое поле и числовой операнд (дробные — только у полей с
  -- 'fieldIsIntegral' = 'False', например ReplayGain).
  | VBetween (FieldRef Scientific) Scientific Scientific
  -- ^ Числовой диапазон.
  | VBool (FieldRef Bool) Bool
  -- ^ Булево поле и булево значение.
  | VRelative (FieldRef Day) RelOp Integer
  -- ^ Датовое поле и сравнение в днях.
  | VDate (FieldRef Day) DateOp Day
  -- ^ Датовое поле, сравнительный оператор и абсолютная дата.
  | VDateRange (FieldRef Day) Day Day
  -- ^ Датовое поле и диапазон абсолютных дат (включительно).
  | VPresence SomeField PresenceOp
  -- ^ Проверка наличия значения поля: обычное поле, поддерживающее
  -- операторы @отсутствует@/@присутствует@ ('fieldPresence').
  | VPlaylist PlaylistMembership PlaylistRef
  -- ^ Членство в подборке: ссылка и требуемое направление
  -- (@inPlaylist@/@notInPlaylist@).
  deriving (Eq, Show)

-- | Текстовые операторы.
data TextOp = TEq | TNe | TContains | TNotContains | TStartsWith | TEndsWith
  deriving (Eq, Show, Enum, Bounded)

-- | Числовые операторы. 'NGe' и 'NLe' (@>=@, @<=@) — «специальные»:
-- в НСП нет таких операторов, поэтому 'Nspeller.Navidrome' разворачивает
-- их в эквивалентные выражения (см. 'Nspeller.Navidrome.toNspCond').
data NumOp = NEq | NNe | NGt | NGe | NLt | NLe
  deriving (Eq, Show, Enum, Bounded)

-- | Относительные сравнения дат.
data RelOp = InTheLast | NotInTheLast
  deriving (Eq, Show, Enum, Bounded)

-- | Операторы сравнения датового поля с абсолютной датой.
--
-- @DGt@/@DLt@ — строгие (@>@, @<@), @DGe@/@DLe@ — включающие
-- (@>=@, @<=@), 'DBefore'/'DAfter' — границы Navidrome
-- (@before@/@after@, строгие же — см. 'Nspeller.Navidrome.toNspCond').
data DateOp
  = DEq
  | DNe
  | DGt
  | DGe
  | DLt
  | DLe
  | DBefore
  | DAfter
  deriving (Eq, Show, Enum, Bounded)

-- | Режим сортировки.
data SortMode
  = SortRandomMode
  | SortBy [SortItem]
  deriving (Eq, Show)

-- | Направление сортировки.
data SortDir = SortAsc | SortDesc
  deriving (Eq, Show, Enum, Bounded)

-- | Элемент сортировки валидированной подборки: DSL-имя поля и
-- направление. Имя канонизировано валидацией
-- ('Nspeller.Fields.sortFieldByName'): поле обязано иметь признак
-- сортировки, сравнение и вывод идут по DSL-имени.
data SortItem = SortItem Text SortDir
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Ошибки компиляции
------------------------------------------------------------------------------

-- | Ошибка компиляции одного файла.
data CompileError
  = CompileError
      { ceFile :: FilePath
      -- ^ Имя файла (выводится в первой строке сообщения).
      , cePos :: Maybe (Int, Int)
      -- ^ Строка и столбец (с единицы); 'Nothing' — ошибка без
      -- позиции (файловая система).
      , ceLineText :: Text
      -- ^ Текст строки с ошибкой (для подсветки).
      , ceSpan :: Int
      -- ^ Ширина подчёркивания в символах (0 — не выводить).
      , ceMessages :: NonEmpty Text
      -- ^ Сообщение (или несколько строк сообщения).
      }
  deriving (Eq, Show)

-- | Форматирует ошибку:
--
-- @
-- broken.mix:5:3
--
--   оценка содержит "rock"
--   ^^^^^^^^^^^^^^^^^^^^^^
--
-- Оператор «содержит» применим только к текстовым полям.
-- Поле «оценка» имеет числовой тип.
-- @
renderCompileError :: CompileError -> Text
renderCompileError (CompileError fp pos lineText spanW msgs) =
  let fileTxt = T.pack fp
   in case pos of
        Nothing ->
          T.intercalate "\n" (fileTxt <> ": " <> NE.head msgs : NE.tail msgs)
        Just (ln, col) ->
          T.intercalate
            "\n"
            ( [ fileTxt <> ":" <> T.pack (show ln) <> ":" <> T.pack (show col)
              , ""
              , lineText
              ]
                <> [T.replicate (col - 1) " " <> T.replicate (max 1 spanW) "^"]
                <> [""]
                <> NE.toList msgs
            )
