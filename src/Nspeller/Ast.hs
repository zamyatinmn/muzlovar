{-# LANGUAGE GADTs #-}
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
-- Поля Navidrome представлены GADT 'Field', индексированным типом
-- значения поля. Это делает некорректные комбинации «поле/оператор»
-- непредставимыми в валидированном AST: конструктор 'VText' принимает
-- только 'Field' 'Text', 'VNumber' — только 'Field' 'Integer' и т. д.
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

    -- * Поля Navidrome
  , Field (..)
  , SomeField (..)
  , fieldName
  , fieldValueType
  , fieldByName
  , fieldPresence
  , PresenceField (..)
  , presenceFieldName
  , presenceValueType
  , boolFieldNames

    -- * Разобранный (невалидированный) AST
  , ParsedFile (..)
  , Statement (..)
  , GroupKind (..)
  , LogicGroup (..)
  , CondItem (..)
  , RawCond (..)
  , RawOp (..)
  , rawOpDesc
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
  , SortMode (..)
  , SortField (..)
  , SortDir (..)
  , SortItem (..)
  , sortFieldName
  , sortFieldByName

    -- * Ошибки компиляции
  , CompileError (..)
  , renderCompileError
  ) where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)

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
-- Категории значений
------------------------------------------------------------------------------

-- | Категория значения, которую принимает поле.
data ValueType
  = TextType
  | NumberType
  | BoolType
  | DateType
  deriving (Eq, Show, Enum, Bounded)

-- | Описание категории для сообщений об ошибках
-- (используется в конструкции «Поле «x» имеет …»).
valueTypeDesc :: ValueType -> Text
valueTypeDesc = \case
  TextType -> "текстовый тип"
  NumberType -> "числовой тип"
  BoolType -> "логический тип"
  DateType -> "тип даты"

-- | Ожидаемый вид значения для сообщений об ошибках
-- (используется в конструкции «ожидается …»).
valueTypeExpect :: ValueType -> Text
valueTypeExpect = \case
  TextType -> "текст"
  NumberType -> "число"
  BoolType -> "«да» или «нет»"
  DateType -> "относительное сравнение «за N дней»"

------------------------------------------------------------------------------
-- Поля
------------------------------------------------------------------------------

-- | Поле Navidrome, индексированное типом его значения.
--
-- Индекс гарантирует, что условие валидированного AST не может
-- сравнить текстовое поле числовым оператором: такой терм нельзя
-- даже написать, не нарушив типы.
data Field a where
  Title :: Field Text
  Album :: Field Text
  Genre :: Field Text
  ExplicitStatus :: Field Text
  Year :: Field Integer
  Rating :: Field Integer
  PlayCount :: Field Integer
  RGTrackGain :: Field Integer
  Loved :: Field Bool
  HasCoverArt :: Field Bool
  LastPlayed :: Field Day
  DateAdded :: Field Day

deriving instance Eq (Field a)

deriving instance Show (Field a)

-- | Поле в динамической упаковке (для таблицы соответствия имён).
data SomeField = forall a. SomeField (Field a)

-- | Каноническое имя поля в документации Navidrome.
fieldName :: Field a -> Text
fieldName = \case
  Title -> "title"
  Album -> "album"
  Genre -> "genre"
  ExplicitStatus -> "explicitstatus"
  Year -> "year"
  Rating -> "rating"
  PlayCount -> "playcount"
  RGTrackGain -> "rgtrackgain"
  Loved -> "loved"
  HasCoverArt -> "hascoverart"
  LastPlayed -> "lastplayed"
  DateAdded -> "dateadded"

-- | Категория значения поля.
fieldValueType :: Field a -> ValueType
fieldValueType = \case
  Title -> TextType
  Album -> TextType
  Genre -> TextType
  ExplicitStatus -> TextType
  Year -> NumberType
  Rating -> NumberType
  PlayCount -> NumberType
  RGTrackGain -> NumberType
  Loved -> BoolType
  HasCoverArt -> BoolType
  LastPlayed -> DateType
  DateAdded -> DateType

-- | Имя поля из DSL → поле.
fieldByName :: Text -> Maybe SomeField
fieldByName name =
  case find matches table of
    Just sf -> Just sf
    Nothing -> Nothing
  where
    table :: [SomeField]
    table =
      [ SomeField Title
      , SomeField Album
      , SomeField Genre
      , SomeField ExplicitStatus
      , SomeField Year
      , SomeField Rating
      , SomeField PlayCount
      , SomeField RGTrackGain
      , SomeField Loved
      , SomeField HasCoverArt
      , SomeField LastPlayed
      , SomeField DateAdded
      ]
    matches sf@(SomeField f) = name == fieldName f || name == dslName sf
    -- Русские имена полей из DSL:
    dslName :: SomeField -> Text
    dslName (SomeField f) = case f of
      Title -> "название"
      Album -> "альбом"
      Genre -> "жанр"
      ExplicitStatus -> "explicit"
      Year -> "год"
      Rating -> "оценка"
      PlayCount -> "прослушиваний"
      RGTrackGain -> "replaygain"
      Loved -> "любимое"
      HasCoverArt -> "обложка"
      LastPlayed -> "последнее_прослушивание"
      DateAdded -> "добавлено"

-- | Поля, поддерживающие операторы 'Absent'/'Present'
-- (согласно документации Navidrome: теговые и текстовые поля,
-- а также числовые поля ReplayGain).
fieldPresence :: Field a -> Maybe PresenceField
fieldPresence = \case
  Album -> Just PAlbum
  Genre -> Just PGenre
  ExplicitStatus -> Just PExplicitStatus
  RGTrackGain -> Just PRGTrackGain
  _ -> Nothing

-- | Поле, поддерживающее проверку наличия.
data PresenceField
  = PAlbum
  | PGenre
  | PExplicitStatus
  | PRGTrackGain
  deriving (Eq, Show, Enum, Bounded)

-- | Имя такого поля в NSP.
presenceFieldName :: PresenceField -> Text
presenceFieldName = \case
  PAlbum -> "album"
  PGenre -> "genre"
  PExplicitStatus -> "explicitstatus"
  PRGTrackGain -> "rgtrackgain"

-- | Категория значения поля, поддерживающего проверку наличия.
presenceValueType :: PresenceField -> ValueType
presenceValueType = \case
  PAlbum -> TextType
  PGenre -> TextType
  PExplicitStatus -> TextType
  PRGTrackGain -> NumberType

-- | Имена булевых полей в DSL (запрещены в сортировке).
boolFieldNames :: [Text]
boolFieldNames = ["любимое", "обложка"]

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
  | RBetween Text Integer Integer
  -- ^ @год между 1980 и 1989@
  | RPresence Text PresenceOp
  -- ^ @replaygain отсутствует@
  | RRelative Text Integer
  -- ^ @добавлено за 30 дней@
  | RNotPlayed Integer
  -- ^ @не звучало 90 дней@
  deriving (Eq, Show)

-- | Бинарные операторы DSL.
data RawOp
  = OpEq
  | OpNe
  | OpGt
  | OpLt
  | OpContains
  | OpNotContains
  | OpStartsWith
  | OpEndsWith
  deriving (Eq, Show, Enum, Bounded)

-- | Написание оператора в DSL (для сообщений об ошибках).
rawOpDesc :: RawOp -> Text
rawOpDesc = \case
  OpEq -> "="
  OpNe -> "!="
  OpGt -> ">"
  OpLt -> "<"
  OpContains -> "содержит"
  OpNotContains -> "не содержит"
  OpStartsWith -> "начинается с"
  OpEndsWith -> "заканчивается на"

-- | Значение-операнд в условии.
data RawValue
  = RVText Text
  | RVNumber Integer
  | RVBool Bool
  deriving (Eq, Show)

-- | Отображение значения в сообщениях об ошибках.
rawValueText :: RawValue -> Text
rawValueText = \case
  RVText t -> t
  RVNumber n -> T.pack (show n)
  RVBool True -> "да"
  RVBool False -> "нет"

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
-- сочетания «тип поля — оператор — тип значения».
data ValidCond
  = VText (Field Text) TextOp Text
  -- ^ Текстовое поле и текстовый операнд.
  | VNumber (Field Integer) NumOp Integer
  -- ^ Числовое поле и числовой операнд.
  | VBetween (Field Integer) Integer Integer
  -- ^ Числовой диапазон.
  | VBool (Field Bool) Bool
  -- ^ Булево поле и булево значение.
  | VRelative (Field Day) RelOp Integer
  -- ^ Датовое поле и сравнение в днях.
  | VPresence PresenceField PresenceOp
  -- ^ Проверка наличия значения поля.
  deriving (Eq, Show)

-- | Текстовые операторы.
data TextOp = TEq | TNe | TContains | TNotContains | TStartsWith | TEndsWith
  deriving (Eq, Show, Enum, Bounded)

-- | Числовые операторы.
data NumOp = NEq | NNe | NGt | NLt
  deriving (Eq, Show, Enum, Bounded)

-- | Относительные сравнения дат.
data RelOp = InTheLast | NotInTheLast
  deriving (Eq, Show, Enum, Bounded)

-- | Режим сортировки.
data SortMode
  = SortRandomMode
  | SortBy [SortItem]
  deriving (Eq, Show)

-- | Поле сортировки (только не-булевые поля).
data SortField
  = SFTitle
  | SFAlbum
  | SFGenre
  | SFYear
  | SFRating
  | SFPlayCount
  | SFLastPlayed
  | SFDateAdded
  | SFExplicitStatus
  | SFReplayGain
  deriving (Eq, Show, Enum, Bounded)

-- | Направление сортировки.
data SortDir = SortAsc | SortDesc
  deriving (Eq, Show, Enum, Bounded)

-- | Элемент сортировки валидированной подборки.
data SortItem = SortItem SortField SortDir
  deriving (Eq, Show)

-- | Имя поля сортировки в NSP.
sortFieldName :: SortField -> Text
sortFieldName = \case
  SFTitle -> "title"
  SFAlbum -> "album"
  SFGenre -> "genre"
  SFYear -> "year"
  SFRating -> "rating"
  SFPlayCount -> "playcount"
  SFLastPlayed -> "lastplayed"
  SFDateAdded -> "dateadded"
  SFExplicitStatus -> "explicitstatus"
  SFReplayGain -> "rgtrackgain"

-- | Имя поля из DSL → поле сортировки.
--
-- Булевые поля сюда намеренно не входят: Navidrome не позволяет
-- сортировать по ним, а валидация даёт отдельное сообщение.
sortFieldByName :: Text -> Maybe SortField
sortFieldByName name =
  case find (\sf -> name == sortFieldName sf || name == dslName sf) candidates of
    Just sf -> Just sf
    Nothing -> Nothing
  where
    candidates = [minBound .. maxBound] :: [SortField]
    dslName :: SortField -> Text
    dslName = \case
      SFTitle -> "название"
      SFAlbum -> "альбом"
      SFGenre -> "жанр"
      SFYear -> "год"
      SFRating -> "оценка"
      SFPlayCount -> "прослушиваний"
      SFLastPlayed -> "последнее_прослушивание"
      SFDateAdded -> "добавлено"
      SFExplicitStatus -> "explicit"
      SFReplayGain -> "replaygain"

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
