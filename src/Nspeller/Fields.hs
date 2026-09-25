{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Реестр полей: спецификации 'FieldSpec' и реестр 'defaultRegistry'.
--
-- Модуль — единственный источник правды о полях: одно поле — одна
-- запись реестра, в которой собраны все его capabilities:
--
-- * имена: DSL ('spDslName'), NSP ('spNspName'), подпись для UI
--   ('spTitle'), категория палитры ('spGroup');
-- * категория значения ('spCategory') и типобезопасный вид поля
--   ('spKind');
-- * возможности ('spCapabilities'): статическое поле, персональное,
--   цель сахара «не звучало N дней»;
-- * операторы с категориями операндов ('spOperators', 'OpId' и
--   'OperandSlot');
-- * признаки: multivalue, целочность, сортировка, наличие;
-- * ограничения значений ('spConstraints'), enum, варианты
--   булевого поля и виды ссылки.
--
-- Каталога конкретных полей в модуле НЕТ: вместо перечисления
-- конструкторов есть ссылка 'FieldRef' — типобезопасная привязка к
-- 'FieldSpec' записи реестра. 'Nspeller.Ast.ValidCond' собирается из
-- таких ссылок, поэтому «текстовое условие поверх числового поля»
-- не представимо в валидированном AST: индекс типа берётся из
-- 'spKind' спецификации, а совпадение по 'FieldKind' даёт валидации
-- свидетельство типа (@a ~ Text@ для 'KindText' и т. п.).
--
-- Все сведения о поле (имена, категория, операторы, признаки,
-- ограничения, enum) лежат в 'FieldSpec'; функции вроде 'fieldName'
-- или 'fieldNumConstraints' — его проекции. 'fieldByName',
-- 'Nspeller.Schema.fieldSchemas' и списки возможностей строятся из
-- 'defaultRegistry': новое поле описывается одной записью реестра,
-- без правки отдельных списков.
module Nspeller.Fields
  ( -- * Категории значений
    ValueType (..)
  , valueTypeDesc
  , valueTypeExpect

    -- * Ссылки на подборки
  , PlaylistRefKind (..)
  , PlaylistRef (..)
  , PlaylistMembership (..)
  , playlistRefDslName
  , playlistRefKindDsl
  , playlistRefKindId

    -- * Ссылки на поля реестра
  , FieldRef (..)
  , SomeField (..)

    -- * Возможности поля
  , FieldCapability (..)
  , specHasCapability
  , fieldHasCapability
  , someFieldHasCapability
  , fieldByCapability
  , capabilityFieldNames

    -- * Вид поля
  , FieldKind (..)
  , kindValueType
  , kindEq
  , fieldKind
  , fieldsOfKind

    -- * Операторы
  , OpId (..)
  , OperandSlot (..)
  , FieldOperator (..)
  , textOperators
  , numberOperators
  , boolOperators
  , dateOperators
  , playlistRefOperators
  , operatorsForKind

    -- * Спецификация поля
  , FieldSpec (..)
  , SomeFieldSpec (..)
  , specNames

    -- * Закрытые наборы и ограничения значений
  , EnumVariant (..)
  , NumConstraints (..)

    -- * Реестр
  , FieldRegistry (..)
  , defaultRegistry
  , fieldSpecs

    -- * Пресеты спецификаций
  , textField
  , numberField
  , boolField
  , dateField
  , refField

    -- * Проекции спецификации
  , fieldName
  , fieldDslName
  , fieldValueType
  , fieldPresence
  , fieldEnum
  , fieldIsIntegral
  , fieldMultivalue
  , fieldNumConstraints
  , fieldByName

    -- * Поля сортировки
  , sortFieldName
  , sortFieldByName
  ) where

import Data.List (find)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Type.Equality ((:~:) (..))

------------------------------------------------------------------------------
-- Категории значений
------------------------------------------------------------------------------

-- | Категория значения, которое принимает поле.
data ValueType
  = TextType
  | NumberType
  | BoolType
  | DateType
  | PlaylistRefType
  -- ^ Ссылка на подборку: объект @{kind, value}@ (@inPlaylist@ и т. п.).
  deriving (Eq, Show, Enum, Bounded)

-- | Описание категории для сообщений об ошибках
-- (используется в конструкции «Поле «x» имеет …»).
valueTypeDesc :: ValueType -> Text
valueTypeDesc = \case
  TextType -> "текстовый тип"
  NumberType -> "числовой тип"
  BoolType -> "логический тип"
  DateType -> "тип даты"
  PlaylistRefType -> "тип ссылки на подборку"

-- | Ожидаемый вид значения для сообщений об ошибках
-- (используется в конструкции «ожидается …»).
valueTypeExpect :: ValueType -> Text
valueTypeExpect = \case
  TextType -> "текст"
  NumberType -> "число"
  BoolType -> "«да» или «нет»"
  DateType -> "дата в формате ГГГГ-ММ-ДД"
  PlaylistRefType -> "ссылка на подборку"

------------------------------------------------------------------------------
-- Ссылки на подборки
------------------------------------------------------------------------------

-- | Вид ссылки на подборку: стабильный идентификатор (@id@) либо
-- путь к файлу подборки (@path@).
data PlaylistRefKind = RefId | RefPath
  deriving (Eq, Show, Enum, Bounded)

-- | Ссылка на подборку: вид и значение (идентификатор или путь).
data PlaylistRef = PlaylistRef
  { prKind :: PlaylistRefKind
  , prValue :: Text
  }
  deriving (Eq, Show)

-- | Участие трека в подборке: входит (@inPlaylist@) или не входит
-- (@notInPlaylist@).
data PlaylistMembership = InPlaylist | NotInPlaylist
  deriving (Eq, Show, Enum, Bounded)

-- | Имя псевдополя ссылки на подборку в DSL и схеме.
playlistRefDslName :: Text
playlistRefDslName = "подборка"

-- | Написание вида ссылки в DSL: @id@ или @файл@.
playlistRefKindDsl :: PlaylistRefKind -> Text
playlistRefKindDsl = \case
  RefId -> "id"
  RefPath -> "файл"

-- | Идентификатор вида ссылки в JSON-схеме и DTO: @id@ или @path@.
playlistRefKindId :: PlaylistRefKind -> Text
playlistRefKindId = \case
  RefId -> "id"
  RefPath -> "path"

------------------------------------------------------------------------------
-- Ссылки на поля реестра
------------------------------------------------------------------------------

-- | Типобезопасная ссылка на поле реестра, индексированная типом его
-- значения.
--
-- Поле не описывается каталогом конструкторов: ссылка создаётся из
-- 'FieldSpec' ('fieldByName', 'fieldsOfKind', 'fieldByCapability') и
-- несёт спецификацию с собой. Индекс @a@ берётся из 'spKind'
-- спецификации, поэтому условие валидированного AST не может
-- сравнить текстовое поле числовым оператором: такой терм нельзя
-- даже написать, не нарушив типы.
newtype FieldRef a = FieldRef
  { refSpec :: FieldSpec a
  -- ^ Спецификация поля, на которое ссылается ссылка.
  }

-- | Равенство ссылок — равенство их имён в реестре: имена уникальны
-- ('specNames' и инварианты реестра), поэтому равенство не зависит
-- от того, из какой копии спецификации ссылка получена.
instance Eq (FieldRef a) where
  a == b = spDslName (refSpec a) == spDslName (refSpec b)

-- | Показывает DSL-имя поля (@fieldRef "название"@): 'Nspeller.Ast.ValidCond'
-- сохраняет читаемый вид 'Show'-вывода.
instance Show (FieldRef a) where
  showsPrec d ref =
    showParen (d > 10) $
      showString "fieldRef " . showsPrec 11 (spDslName (refSpec ref))

-- | Ссылка в динамической упаковке: там, где тип поля не важен
-- ('Nspeller.Ast.VPresence', 'fieldByName').
data SomeField = forall a. SomeField (FieldRef a)

-- | Упакованные ссылки сравниваются по DSL-имени: имена уникальны в
-- реестре 'defaultRegistry', поэтому равенство не зависит от
-- упакованного типа.
instance Eq SomeField where
  SomeField a == SomeField b = spDslName (refSpec a) == spDslName (refSpec b)

-- | Показывает упакованную ссылку (@fieldRef "название"@):
-- 'Nspeller.Ast.ValidCond' сохраняет привычный вид 'Show'-вывода.
instance Show SomeField where
  showsPrec d (SomeField f) = showsPrec d f

------------------------------------------------------------------------------
-- Возможности поля
------------------------------------------------------------------------------

-- | Возможность поля сверх вида значения и набора операторов.
--
-- Возможности — это то, что раньше приходилось искать по конкретным
-- полям: они объявляются в 'spCapabilities' записи реестра, а
-- потребители запрашивают их у реестра ('capabilityFieldNames',
-- 'fieldByCapability'), не зная имён полей.
data FieldCapability
  = CapStatic
    -- ^ Настоящее статическое поле: его имя разрешается в условии
    -- ('fieldByName'). Псевдополя («подборка») такой возможности не
    -- имеют: членство записывается формой «в подборке …» и имени
    -- поля не требует.
  | CapPersonal
    -- ^ Поле входит в список персональных @/api/schema@
    -- ('Nspeller.Schema.personalFields'): его значения зависят от
    -- пользователя Navidrome.
  | CapNotPlayed
    -- ^ Поле — цель DSL-сахара «не звучало N дней»
    -- ('Nspeller.Ast.RNotPlayed'): в этой форме имя поля не
    -- записывается, поэтому валидация берёт его из реестра.
  deriving (Eq, Show, Enum, Bounded)

-- | Есть ли у спецификации возможность.
specHasCapability :: FieldCapability -> FieldSpec a -> Bool
specHasCapability cap spec = cap `elem` spCapabilities spec

-- | Есть ли у поля (ссылки) возможность.
fieldHasCapability :: FieldCapability -> FieldRef a -> Bool
fieldHasCapability cap = specHasCapability cap . refSpec

-- | Есть ли у упакованной ссылки возможность.
someFieldHasCapability :: FieldCapability -> SomeField -> Bool
someFieldHasCapability cap (SomeField f) = fieldHasCapability cap f

-- | Первое поле реестра с указанной возможностью (в порядке
-- DSL-палитры) в динамической упаковке; 'Nothing' — возможности не
-- дал никто. Инвариант реестра (см. тесты): у каждой возможности
-- есть поле, а у 'CapNotPlayed' — ровно одно датовое статическое
-- поле.
fieldByCapability :: FieldCapability -> Maybe SomeField
fieldByCapability cap = case candidates of
  (f : _) -> Just f
  [] -> Nothing
  where
    candidates =
      [ SomeField (FieldRef spec)
      | SomeFieldSpec spec <- registrySpecs defaultRegistry
      , specHasCapability cap spec
      ]

-- | DSL-имена полей с возможностью в порядке реестра: проекция
-- возможностей для @/api/schema@ ('Nspeller.Schema.personalFields'),
-- без перечисления конкретных полей.
capabilityFieldNames :: FieldCapability -> [Text]
capabilityFieldNames cap =
  [ spDslName spec
  | SomeFieldSpec spec <- registrySpecs defaultRegistry
  , specHasCapability cap spec
  ]

------------------------------------------------------------------------------
-- Вид поля
------------------------------------------------------------------------------

-- | Вид поля: индексированный типом значения discriminator.
--
-- Совпадение по 'FieldKind' даёт валидации доказательство типа
-- (@a ~ Text@ для 'KindText' и т. п.), поэтому точки сборки
-- 'Nspeller.Ast.ValidCond' обходятся без перечисления полей реестра.
data FieldKind a where
  KindText :: FieldKind Text
  KindNumber :: FieldKind Scientific
  KindBool :: FieldKind Bool
  KindDate :: FieldKind Day
  KindPlaylistRef :: FieldKind PlaylistRef
  -- ^ Псевдополя «подборка»: статического поля с таким видом нет.

-- | Категория значения вида ('ValueType') — её же JSON-код и тексты
-- сообщений об ошибках.
kindValueType :: FieldKind a -> ValueType
kindValueType = \case
  KindText -> TextType
  KindNumber -> NumberType
  KindBool -> BoolType
  KindDate -> DateType
  KindPlaylistRef -> PlaylistRefType

-- | Сравнение свидетельств вида: 'Just' 'Refl' — виды совпадают
-- (и типы индексов равны), 'Nothing' — виды различаются. По нему
-- 'fieldsOfKind' выбирает из реестра поля того вида, который
-- нужен вызывающему коду, сохраняя тип индекса.
kindEq :: FieldKind a -> FieldKind b -> Maybe (a :~: b)
kindEq KindText KindText = Just Refl
kindEq KindNumber KindNumber = Just Refl
kindEq KindBool KindBool = Just Refl
kindEq KindDate KindDate = Just Refl
kindEq KindPlaylistRef KindPlaylistRef = Just Refl
kindEq _ _ = Nothing

-- | Вид поля ('FieldKind' его спецификации).
fieldKind :: FieldRef a -> FieldKind a
fieldKind = spKind . refSpec

-- | Все поля реестра указанного вида в порядке DSL-палитры.
--
-- Единственный способ выбрать поля по виду: генераторы и тесты
-- получают их из реестра, а не из собственных списков.
fieldsOfKind :: FieldKind a -> [FieldRef a]
fieldsOfKind kind =
  [ FieldRef spec
  | SomeFieldSpec spec <- registrySpecs defaultRegistry
  , Just Refl <- [kindEq kind (spKind spec)]
  ]

------------------------------------------------------------------------------
-- Операторы
------------------------------------------------------------------------------

-- | Идентификатор оператора DSL: ключ @id@ каталога операторов
-- @/api/schema@ (@eq@, @contains@, @inTheLast@ …).
newtype OpId = OpId {unOpId :: Text}
  deriving (Eq, Show)

-- | Категория операнда оператора (слот редактора значения в UI):
-- @text@, @number@, @numberRange@, @bool@, @none@, @days@, @date@,
-- @dateRange@, @playlistRef@.
newtype OperandSlot = OperandSlot {unOperandSlot :: Text}
  deriving (Eq, Show)

-- | Оператор, допустимый для поля: ссылка на каталог операторов
-- плюс категория его операнда.
data FieldOperator = FieldOperator
  { fopId :: OpId
  , fopSlot :: OperandSlot
  }
  deriving (Eq, Show)

-- | Оператор по идентификатору и слоту операнда.
fieldOp :: Text -> Text -> FieldOperator
fieldOp i s = FieldOperator (OpId i) (OperandSlot s)

-- | Операторы текстового поля (слот операнда — текст).
textOperators :: [FieldOperator]
textOperators =
  [ fieldOp "eq" "text"
  , fieldOp "ne" "text"
  , fieldOp "contains" "text"
  , fieldOp "notContains" "text"
  , fieldOp "startsWith" "text"
  , fieldOp "endsWith" "text"
  ]

-- | Операторы числового поля (равенство, сравнение и диапазон).
numberOperators :: [FieldOperator]
numberOperators =
  [ fieldOp "eq" "number"
  , fieldOp "ne" "number"
  , fieldOp "gt" "number"
  , fieldOp "ge" "number"
  , fieldOp "lt" "number"
  , fieldOp "le" "number"
  , fieldOp "between" "numberRange"
  ]

-- | Операторы логического поля: равенство и сахар «флаг».
boolOperators :: [FieldOperator]
boolOperators =
  [ fieldOp "eq" "bool"
  , fieldOp "ne" "bool"
  , fieldOp "bare" "none"
  ]

-- | Операторы датового поля: относительное сравнение в днях плюс
-- абсолютные даты — сравнение, границы @до@/@после@ и диапазон.
-- @inTheLast@ идёт первым: это оператор по умолчанию
-- ('Nspeller.Validation' и UI добавляют первым именно его).
dateOperators :: [FieldOperator]
dateOperators =
  [ fieldOp "inTheLast" "days"
  , fieldOp "notInTheLast" "days"
  , fieldOp "eq" "date"
  , fieldOp "ne" "date"
  , fieldOp "gt" "date"
  , fieldOp "ge" "date"
  , fieldOp "lt" "date"
  , fieldOp "le" "date"
  , fieldOp "before" "date"
  , fieldOp "after" "date"
  , fieldOp "between" "dateRange"
  ]

-- | Операторы псевдополя «подборка» (вид ссылки + строка).
playlistRefOperators :: [FieldOperator]
playlistRefOperators =
  [ fieldOp "inPlaylist" "playlistRef"
  , fieldOp "notInPlaylist" "playlistRef"
  ]

-- | Операторы, допустимые для вида поля: вид, а не конкретное поле,
-- выбирает набор (пресеты спецификаций берут его отсюда).
operatorsForKind :: FieldKind a -> [FieldOperator]
operatorsForKind = \case
  KindText -> textOperators
  KindNumber -> numberOperators
  KindBool -> boolOperators
  KindDate -> dateOperators
  KindPlaylistRef -> playlistRefOperators

------------------------------------------------------------------------------
-- Спецификация поля
------------------------------------------------------------------------------

-- | Спецификация поля: capabilities из таблицы metadata аудита.
--
-- Поле с индексом @a@ знает свой вид ('spKind'), а значит, тип
-- своего значения; ссылка 'FieldRef' на это поле получает индекс из
-- того же 'spKind'.
data FieldSpec a = FieldSpec
  { spKind :: FieldKind a
  -- ^ Вид поля: по нему валидация выбирает разбор операнда.
  , spCategory :: ValueType
  -- ^ Категория значения ('kindValueType' вида): текст, число,
  -- логическое, дата, ссылка на подборку.
  , spCapabilities :: [FieldCapability]
  -- ^ Возможности поля ('FieldCapability'): статическое, цели
  -- сахара «не звучало», персональное.
  , spGroup :: Text
  -- ^ Категория (группа ингредиентов) в палитре: @logic@, @history@,
  -- @meta@, @audio@, @files@, @album@, @artist@, @ids@, @links@.
  , spDslName :: Text
  -- ^ Имя поля в DSL (принимает парсер и 'fieldByName').
  , spNspName :: Text
  -- ^ Каноническое имя поля в документации Navidrome (JSON @.nsp@).
  , spAliases :: [Text]
  -- ^ Дополнительные имена, по которым поле разрешается в реестре
  -- ('specNames'): задокументированные альтернативные написания
  -- Navidrome (например @replaygain_track_gain@ или @lastPlayed@).
  -- Алиас — только способ найти запись: выводится всегда
  -- 'spNspName'/'spDslName'. Пусто у полей без альтернативных имён.
  , spTitle :: Text
  -- ^ Русская подпись поля для интерфейса.
  , spOperators :: [FieldOperator]
  -- ^ Операторы, допустимые для поля, со слотами операндов.
  , spMultivalue :: Bool
  -- ^ Поле multivalue: условие описывает одно из нескольких значений.
  , spIntegral :: Bool
  -- ^ Значения поля целые: дробный операнд — ошибка валидации.
  , spSortable :: Bool
  -- ^ Поле участвует в сортировке ('sortFieldByName').
  , spPresence :: Bool
  -- ^ Поле поддерживает операторы @отсутствует@/@присутствует@
  -- ('fieldPresence').
  , spConstraints :: Maybe NumConstraints
  -- ^ Известные границы значений ('Nothing' — область не ограничена).
  , spEnum :: Maybe [EnumVariant]
  -- ^ Закрытый набор значений ('Nothing' — значения свободны).
  , spValueVariants :: Maybe [Text]
  -- ^ Закрытый набор подписей для редактора («да»/«нет» у флагов).
  , spRefKinds :: Maybe [EnumVariant]
  -- ^ Виды ссылки для типа @playlistRef@ ('Nothing' у обычных полей).
  }

-- | Спецификация в динамической упаковке: запись реестра
-- 'defaultRegistry'.
data SomeFieldSpec = forall a. SomeFieldSpec (FieldSpec a)

-- | Все имена, по которым поле разрешается в реестре ('specByName'):
-- DSL-имя, каноническое NSP-имя и алиасы ('spAliases'). Список
-- обязан быть уникален в пределах реестра (инвариант проверяется
-- тестами), поэтому алиас не может совпасть ни с чьим DSL-, NSP- или
-- другим алиас-именем; после приведения к нижнему регистру имена
-- разных записей тоже не пересекаются — иначе поиск без учёта
-- регистра стал бы неоднозначным.
specNames :: FieldSpec a -> [Text]
specNames spec = spDslName spec : spNspName spec : spAliases spec

------------------------------------------------------------------------------
-- Пресеты спецификаций
------------------------------------------------------------------------------

-- | Пресет текстового поля: имя в DSL, имя в NSP, подпись для UI и
-- категория палитры. Остальные capabilities — значения по умолчанию
-- текстового статического поля.
textField :: Text -> Text -> Text -> Text -> FieldSpec Text
textField dsl nsp title group = FieldSpec
  { spKind = KindText
  , spCategory = TextType
  , spCapabilities = [CapStatic]
  , spGroup = group
  , spDslName = dsl
  , spNspName = nsp
  , spAliases = []
  , spTitle = title
  , spOperators = operatorsForKind KindText
  , spMultivalue = False
  , spIntegral = True
  , spSortable = True
  , spPresence = False
  , spConstraints = Nothing
  , spEnum = Nothing
  , spValueVariants = Nothing
  , spRefKinds = Nothing
  }

-- | Пресет числового поля. Границы намеренно не заданы: жёстких
-- пределов у годовых и ReplayGain полей нет, а ограничения рейтинга
-- и числа прослушиваний задаются обновлением 'spConstraints'.
numberField :: Text -> Text -> Text -> Text -> FieldSpec Scientific
numberField dsl nsp title group = FieldSpec
  { spKind = KindNumber
  , spCategory = NumberType
  , spCapabilities = [CapStatic]
  , spGroup = group
  , spDslName = dsl
  , spNspName = nsp
  , spAliases = []
  , spTitle = title
  , spOperators = operatorsForKind KindNumber
  , spMultivalue = False
  , spIntegral = True
  , spSortable = True
  , spPresence = False
  , spConstraints = Nothing
  , spEnum = Nothing
  , spValueVariants = Nothing
  , spRefKinds = Nothing
  }

-- | Пресет логического поля: закрытый набор подписей «да»/«нет» и
-- сахар «флаг» (@bare@) в наборе операторов.
boolField :: Text -> Text -> Text -> Text -> FieldSpec Bool
boolField dsl nsp title group = FieldSpec
  { spKind = KindBool
  , spCategory = BoolType
  , spCapabilities = [CapStatic]
  , spGroup = group
  , spDslName = dsl
  , spNspName = nsp
  , spAliases = []
  , spTitle = title
  , spOperators = operatorsForKind KindBool
  , spMultivalue = False
  , spIntegral = True
  , spSortable = True
  , spPresence = False
  , spConstraints = Nothing
  , spEnum = Nothing
  , spValueVariants = Just ["да", "нет"]
  , spRefKinds = Nothing
  }

-- | Пресет датового поля.
dateField :: Text -> Text -> Text -> Text -> FieldSpec Day
dateField dsl nsp title group = FieldSpec
  { spKind = KindDate
  , spCategory = DateType
  , spCapabilities = [CapStatic]
  , spGroup = group
  , spDslName = dsl
  , spNspName = nsp
  , spAliases = []
  , spTitle = title
  , spOperators = operatorsForKind KindDate
  , spMultivalue = False
  , spIntegral = True
  , spSortable = True
  , spPresence = False
  , spConstraints = Nothing
  , spEnum = Nothing
  , spValueVariants = Nothing
  , spRefKinds = Nothing
  }

-- | Пресет псевдополя ссылки на подборку: возможности 'CapStatic'
-- нет (имя псевдополя не разрешается в условии), сортировка
-- выключена, виды ссылки (@id@ / путь к файлу) заданы для
-- редактора.
refField :: Text -> Text -> Text -> Text -> FieldSpec PlaylistRef
refField dsl nsp title group = FieldSpec
  { spKind = KindPlaylistRef
  , spCategory = PlaylistRefType
  , spCapabilities = []
  , spGroup = group
  , spDslName = dsl
  , spNspName = nsp
  , spAliases = []
  , spTitle = title
  , spOperators = operatorsForKind KindPlaylistRef
  , spMultivalue = False
  , spIntegral = True
  , spSortable = False
  , spPresence = False
  , spConstraints = Nothing
  , spEnum = Nothing
  , spValueVariants = Nothing
  , spRefKinds = Just [EnumVariant "id" "ID", EnumVariant "path" "путь к файлу"]
  }

------------------------------------------------------------------------------
-- Спецификации полей реестра
------------------------------------------------------------------------------
--
-- Состав соответствует таблице Fields документации Navidrome
-- (model/criteria/fields.go): 71 статическое поле плюс псевдополе
-- «подборка». Записи сгруппированы по категориям палитры; новое поле
-- описывается только записью здесь — без правок валидации, схемы или
-- интерфейса.

-- | Ограничения «не меньше границы» с шагом 1: размеры, частоты,
-- счётчики треков.
minStep1 :: Scientific -> NumConstraints
minStep1 lo = NumConstraints (Just lo) Nothing (Just 1)

-- | Ограничение «не меньше границы» без шага: длительности в
-- секундах хранятся дробными.
minOnly :: Scientific -> NumConstraints
minOnly lo = NumConstraints (Just lo) Nothing Nothing

-- | Рейтинг 0..5 с шагом 1 — тот же столбец annotation.rating, что и
-- у трека (так документирован rating и albumrating).
ratingBounds :: NumConstraints
ratingBounds = NumConstraints (Just 0) (Just 5) (Just 1)

------------------------------------------------------------------------------
-- Группа «Логика»
------------------------------------------------------------------------------

lovedSpec :: FieldSpec Bool
lovedSpec =
  (boolField "любимое" "loved" "Любимое" "logic")
    { spCapabilities = [CapStatic, CapPersonal]
    }

ratingSpec :: FieldSpec Scientific
ratingSpec =
  (numberField "оценка" "rating" "Оценка" "logic")
    { spConstraints = Just ratingBounds
    , spCapabilities = [CapStatic, CapPersonal]
    }

-- | «Средняя оценка» — усреднённый рейтинг по всем пользователям,
-- дробный и без границ.
averageRatingSpec :: FieldSpec Scientific
averageRatingSpec =
  (numberField "средняя_оценка" "averagerating" "Средняя оценка" "logic")
    { spIntegral = False
    }

hasCoverArtSpec :: FieldSpec Bool
hasCoverArtSpec = boolField "обложка" "hascoverart" "Обложка" "logic"

compilationSpec :: FieldSpec Bool
compilationSpec = boolField "сборник" "compilation" "Сборник" "logic"

------------------------------------------------------------------------------
-- Группа «История»
------------------------------------------------------------------------------

playCountSpec :: FieldSpec Scientific
playCountSpec =
  (numberField "прослушиваний" "playcount" "Прослушивания" "history")
    { spConstraints = Just (minStep1 0)
    , spCapabilities = [CapStatic, CapPersonal]
    , spAliases = ["playCount"]
    }

-- | «Последнее прослушивание» — цель сахара «не звучало N дней»
-- ('CapNotPlayed'): сам сахар в DSL имени поля не записывает.
lastPlayedSpec :: FieldSpec Day
lastPlayedSpec =
  (dateField "последнее_прослушивание" "lastplayed" "Последнее прослушивание" "history")
    { spCapabilities = [CapStatic, CapNotPlayed, CapPersonal]
    , spAliases = ["lastPlayed"]
    }

dateAddedSpec :: FieldSpec Day
dateAddedSpec = dateField "добавлено" "dateadded" "Добавлено" "history"

-- | Дата, когда трек стал любимым; camelCase-алиас @dateLoved@ взят
-- из примеров документации Navidrome.
dateLovedSpec :: FieldSpec Day
dateLovedSpec =
  (dateField "дата_любимого" "dateloved" "Дата любимого" "history")
    { spCapabilities = [CapStatic, CapPersonal]
    , spAliases = ["dateLoved"]
    }

dateRatedSpec :: FieldSpec Day
dateRatedSpec =
  (dateField "дата_оценки" "daterated" "Дата оценки" "history")
    { spCapabilities = [CapStatic, CapPersonal]
    }

------------------------------------------------------------------------------
-- Группа «Метаданные»
------------------------------------------------------------------------------

titleSpec :: FieldSpec Text
titleSpec = textField "название" "title" "Название" "meta"

albumSpec :: FieldSpec Text
albumSpec =
  (textField "альбом" "album" "Альбом" "meta")
    { spPresence = True
    }

genreSpec :: FieldSpec Text
genreSpec =
  (textField "жанр" "genre" "Жанр" "meta")
    { spMultivalue = True
    , spPresence = True
    }

-- | «Год» — единственный год с ограничениями: min=0 (отрицательных
-- годов в каталоге не бывает), step=1, максимум не задан — как и у
-- 'originalYearSpec'/'releaseYearSpec', у которых ограничений нет вовсе
-- (см. 'minStep1'). Ограничение идёт одной цепочкой: реестр →
-- 'fieldNumConstraints' → валидация ('Nspeller.Validation.domainError')
-- и @/api/schema@ → атрибуты input/clamp в редакторе.
yearSpec :: FieldSpec Scientific
yearSpec =
  (numberField "год" "year" "Год" "meta")
    { spConstraints = Just (minStep1 0)
    }

explicitSpec :: FieldSpec Text
explicitSpec =
  (textField "explicit" "explicitstatus" "Explicit" "meta")
    { spPresence = True
    , spEnum =
        Just
          [ EnumVariant "e" "Explicit"
          , EnumVariant "c" "Clean"
          , EnumVariant "" "Не определено"
          ]
    }

dateSpec :: FieldSpec Day
dateSpec = dateField "дата_записи" "date" "Дата записи" "meta"

originalYearSpec :: FieldSpec Scientific
originalYearSpec = numberField "оригинальный_год" "originalyear" "Оригинальный год" "meta"

originalDateSpec :: FieldSpec Day
originalDateSpec = dateField "оригинальная_дата" "originaldate" "Оригинальная дата" "meta"

releaseYearSpec :: FieldSpec Scientific
releaseYearSpec = numberField "год_релиза" "releaseyear" "Год релиза" "meta"

releaseDateSpec :: FieldSpec Day
releaseDateSpec = dateField "дата_релиза" "releasedate" "Дата релиза" "meta"

trackNumberSpec :: FieldSpec Scientific
trackNumberSpec =
  (numberField "номер_трека" "tracknumber" "Номер трека" "meta")
    { spConstraints = Just (minStep1 0)
    }

discNumberSpec :: FieldSpec Scientific
discNumberSpec =
  (numberField "номер_диска" "discnumber" "Номер диска" "meta")
    { spConstraints = Just (minStep1 0)
    }

discSubtitleSpec :: FieldSpec Text
discSubtitleSpec =
  (textField "подзаголовок_диска" "discsubtitle" "Подзаголовок диска" "meta")
    { spPresence = True
    }

commentSpec :: FieldSpec Text
commentSpec =
  (textField "комментарий" "comment" "Комментарий" "meta")
    { spPresence = True
    }

lyricsSpec :: FieldSpec Text
lyricsSpec =
  (textField "текст_песни" "lyrics" "Текст песни" "meta")
    { spPresence = True
    }

sortTitleSpec :: FieldSpec Text
sortTitleSpec =
  (textField "сортировка_названия" "sorttitle" "Название для сортировки" "meta")
    { spPresence = True
    }

sortAlbumSpec :: FieldSpec Text
sortAlbumSpec =
  (textField "сортировка_альбома" "sortalbum" "Альбом для сортировки" "meta")
    { spPresence = True
    }

sortArtistSpec :: FieldSpec Text
sortArtistSpec =
  (textField "сортировка_артиста" "sortartist" "Артист для сортировки" "meta")
    { spPresence = True
    }

sortAlbumArtistSpec :: FieldSpec Text
sortAlbumArtistSpec =
  (textField "сортировка_альбомного_артиста" "sortalbumartist" "Альбомный артист для сортировки" "meta")
    { spPresence = True
    }

catalogNumberSpec :: FieldSpec Text
catalogNumberSpec =
  (textField "номер_в_каталоге" "catalognumber" "Номер в каталоге" "meta")
    { spPresence = True
    }

-- | ReplayGain дробный ('spIntegral' = 'False'): шаг и границы не
-- выдумываются, поле поддерживает проверку наличия. Алиас — теговое
-- имя из документации Navidrome (@replaygain_track_gain@);
-- выводится всегда каноническое @rgtrackgain@.
replayGainSpec :: FieldSpec Scientific
replayGainSpec =
  (numberField "replaygain" "rgtrackgain" "ReplayGain" "meta")
    { spIntegral = False
    , spPresence = True
    , spAliases = ["replaygain_track_gain"]
    }

replayGainTrackPeakSpec :: FieldSpec Scientific
replayGainTrackPeakSpec =
  (numberField "replaygain_пик" "rgtrackpeak" "ReplayGain пик" "meta")
    { spIntegral = False
    , spPresence = True
    , spAliases = ["replaygain_track_peak"]
    }

replayGainAlbumGainSpec :: FieldSpec Scientific
replayGainAlbumGainSpec =
  (numberField "replaygain_альбом" "rgalbumgain" "ReplayGain альбом" "meta")
    { spIntegral = False
    , spPresence = True
    , spAliases = ["replaygain_album_gain"]
    }

replayGainAlbumPeakSpec :: FieldSpec Scientific
replayGainAlbumPeakSpec =
  (numberField "replaygain_пик_альбом" "rgalbumpeak" "ReplayGain пик альбома" "meta")
    { spIntegral = False
    , spPresence = True
    , spAliases = ["replaygain_album_peak"]
    }

------------------------------------------------------------------------------
-- Группа «Аудио»
------------------------------------------------------------------------------

-- | Длительность трека в секундах: Navidrome хранит дробным.
durationSpec :: FieldSpec Scientific
durationSpec =
  (numberField "длительность" "duration" "Длительность" "audio")
    { spIntegral = False
    , spConstraints = Just (minOnly 0)
    }

codecSpec :: FieldSpec Text
codecSpec = textField "кодек" "codec" "Кодек" "audio"

bitRateSpec :: FieldSpec Scientific
bitRateSpec =
  (numberField "битрейт" "bitrate" "Битрейт" "audio")
    { spConstraints = Just (minStep1 0)
    }

-- | Битовая глубина: у lossy-форматов её нет, поэтому поддерживает
-- проверку наличия (как в документации Navidrome).
bitDepthSpec :: FieldSpec Scientific
bitDepthSpec =
  (numberField "битовая_глубина" "bitdepth" "Битовая глубина" "audio")
    { spConstraints = Just (minStep1 0)
    , spPresence = True
    }

sampleRateSpec :: FieldSpec Scientific
sampleRateSpec =
  (numberField "частота_дискретизации" "samplerate" "Частота дискретизации" "audio")
    { spConstraints = Just (minStep1 0)
    }

bpmSpec :: FieldSpec Scientific
bpmSpec =
  (numberField "темп" "bpm" "Темп" "audio")
    { spConstraints = Just (minStep1 0)
    , spPresence = True
    }

channelsSpec :: FieldSpec Scientific
channelsSpec =
  (numberField "каналы" "channels" "Каналы" "audio")
    { spConstraints = Just (minStep1 0)
    }

------------------------------------------------------------------------------
-- Группа «Файлы»
------------------------------------------------------------------------------

filePathSpec :: FieldSpec Text
filePathSpec = textField "путь_к_файлу" "filepath" "Путь к файлу" "files"

fileTypeSpec :: FieldSpec Text
fileTypeSpec = textField "тип_файла" "filetype" "Тип файла" "files"

sizeSpec :: FieldSpec Scientific
sizeSpec =
  (numberField "размер" "size" "Размер" "files")
    { spConstraints = Just (minStep1 0)
    }

dateModifiedSpec :: FieldSpec Day
dateModifiedSpec = dateField "изменено" "datemodified" "Изменено" "files"

missingSpec :: FieldSpec Bool
missingSpec = boolField "файл_отсутствует" "missing" "Файл отсутствует" "files"

------------------------------------------------------------------------------
-- Группа «Альбом»
------------------------------------------------------------------------------

albumCommentSpec :: FieldSpec Text
albumCommentSpec =
  (textField "комментарий_альбома" "albumcomment" "Комментарий альбома" "album")
    { spPresence = True
    }

albumRatingSpec :: FieldSpec Scientific
albumRatingSpec =
  (numberField "оценка_альбома" "albumrating" "Оценка альбома" "album")
    { spConstraints = Just ratingBounds
    , spCapabilities = [CapStatic, CapPersonal]
    }

albumLovedSpec :: FieldSpec Bool
albumLovedSpec =
  (boolField "любимый_альбом" "albumloved" "Любимый альбом" "album")
    { spCapabilities = [CapStatic, CapPersonal]
    }

albumPlayCountSpec :: FieldSpec Scientific
albumPlayCountSpec =
  (numberField "прослушиваний_альбома" "albumplaycount" "Прослушиваний альбома" "album")
    { spConstraints = Just (minStep1 0)
    , spCapabilities = [CapStatic, CapPersonal]
    }

albumLastPlayedSpec :: FieldSpec Day
albumLastPlayedSpec =
  (dateField "последнее_прослушивание_альбома" "albumlastplayed" "Последнее прослушивание альбома" "album")
    { spCapabilities = [CapStatic, CapPersonal]
    }

albumDateLovedSpec :: FieldSpec Day
albumDateLovedSpec =
  (dateField "дата_любимого_альбома" "albumdateloved" "Дата любимого альбома" "album")
    { spCapabilities = [CapStatic, CapPersonal]
    }

albumDateRatedSpec :: FieldSpec Day
albumDateRatedSpec =
  (dateField "дата_оценки_альбома" "albumdaterated" "Дата оценки альбома" "album")
    { spCapabilities = [CapStatic, CapPersonal]
    }

albumDateAddedSpec :: FieldSpec Day
albumDateAddedSpec = dateField "дата_добавления_альбома" "albumdateadded" "Дата добавления альбома" "album"

albumDateModifiedSpec :: FieldSpec Day
albumDateModifiedSpec = dateField "дата_изменения_альбома" "albumdatemodified" "Дата изменения альбома" "album"

-- | Суммарная длительность альбома: та же шкала секунд, что и у
-- трека, — дробная.
albumDurationSpec :: FieldSpec Scientific
albumDurationSpec =
  (numberField "длительность_альбома" "albumduration" "Длительность альбома" "album")
    { spIntegral = False
    , spConstraints = Just (minOnly 0)
    }

albumSongCountSpec :: FieldSpec Scientific
albumSongCountSpec =
  (numberField "треков_в_альбоме" "albumsongcount" "Треков в альбоме" "album")
    { spConstraints = Just (minStep1 0)
    }

albumSizeSpec :: FieldSpec Scientific
albumSizeSpec =
  (numberField "размер_альбома" "albumsize" "Размер альбома" "album")
    { spConstraints = Just (minStep1 0)
    }

------------------------------------------------------------------------------
-- Группа «Артист»
------------------------------------------------------------------------------

artistRatingSpec :: FieldSpec Scientific
artistRatingSpec =
  (numberField "оценка_артиста" "artistrating" "Оценка артиста" "artist")
    { spConstraints = Just ratingBounds
    , spCapabilities = [CapStatic, CapPersonal]
    }

artistLovedSpec :: FieldSpec Bool
artistLovedSpec =
  (boolField "любимый_артист" "artistloved" "Любимый артист" "artist")
    { spCapabilities = [CapStatic, CapPersonal]
    }

artistPlayCountSpec :: FieldSpec Scientific
artistPlayCountSpec =
  (numberField "прослушиваний_артиста" "artistplaycount" "Прослушиваний артиста" "artist")
    { spConstraints = Just (minStep1 0)
    , spCapabilities = [CapStatic, CapPersonal]
    }

artistLastPlayedSpec :: FieldSpec Day
artistLastPlayedSpec =
  (dateField "последнее_прослушивание_артиста" "artistlastplayed" "Последнее прослушивание артиста" "artist")
    { spCapabilities = [CapStatic, CapPersonal]
    }

artistDateLovedSpec :: FieldSpec Day
artistDateLovedSpec =
  (dateField "дата_любимого_артиста" "artistdateloved" "Дата любимого артиста" "artist")
    { spCapabilities = [CapStatic, CapPersonal]
    }

artistDateRatedSpec :: FieldSpec Day
artistDateRatedSpec =
  (dateField "дата_оценки_артиста" "artistdaterated" "Дата оценки артиста" "artist")
    { spCapabilities = [CapStatic, CapPersonal]
    }

------------------------------------------------------------------------------
-- Группа «Идентификаторы»
------------------------------------------------------------------------------

mbzAlbumIdSpec :: FieldSpec Text
mbzAlbumIdSpec =
  (textField "mbid_альбома" "mbz_album_id" "MusicBrainz ID альбома" "ids")
    { spPresence = True
    }

mbzAlbumArtistIdSpec :: FieldSpec Text
mbzAlbumArtistIdSpec =
  (textField "mbid_альбомного_артиста" "mbz_album_artist_id" "MusicBrainz ID альбомного артиста" "ids")
    { spPresence = True
    }

mbzArtistIdSpec :: FieldSpec Text
mbzArtistIdSpec =
  (textField "mbid_артиста" "mbz_artist_id" "MusicBrainz ID артиста" "ids")
    { spPresence = True
    }

mbzRecordingIdSpec :: FieldSpec Text
mbzRecordingIdSpec =
  (textField "mbid_записи" "mbz_recording_id" "MusicBrainz ID записи" "ids")
    { spPresence = True
    }

mbzReleaseTrackIdSpec :: FieldSpec Text
mbzReleaseTrackIdSpec =
  (textField "mbid_трека_релиза" "mbz_release_track_id" "MusicBrainz ID трека релиза" "ids")
    { spPresence = True
    }

mbzReleaseGroupIdSpec :: FieldSpec Text
mbzReleaseGroupIdSpec =
  (textField "mbid_группы_релизов" "mbz_release_group_id" "MusicBrainz ID группы релизов" "ids")
    { spPresence = True
    }

libraryIdSpec :: FieldSpec Scientific
libraryIdSpec = numberField "библиотека" "library_id" "Библиотека" "ids"

------------------------------------------------------------------------------
-- Группа «Ссылки»
------------------------------------------------------------------------------

-- | Псевдополе «подборка»: условие членства не привязано к полю
-- реестра, но для палитры и редактора описывается той же
-- 'FieldSpec'.
playlistRefSpec :: FieldSpec PlaylistRef
playlistRefSpec = refField "подборка" "inPlaylist" "Подборка" "links"

------------------------------------------------------------------------------
-- Реестр
------------------------------------------------------------------------------

-- | Реестр полей. Дефолтный реестр 'defaultRegistry' содержит ровно
-- 72 записи: 71 статическое поле из таблицы Fields документации
-- Navidrome плюс псевдополе «подборка». Записи идут по категориям
-- палитры (см. 'spGroup'): «Логика», «История», «Метаданные»,
-- «Аудио», «Файлы», «Альбом», «Артист», «Идентификаторы», «Ссылки».
newtype FieldRegistry = FieldRegistry
  { registrySpecs :: [SomeFieldSpec]
  -- ^ Записи реестра в порядке DSL-палитры.
  }

-- | Дефолтный реестр: единственный источник правды о полях. Из него
-- строятся 'fieldByName', 'fieldsOfKind', именованные проекции
-- сортировки, списки возможностей и 'Nspeller.Schema.fieldSchemas'.
defaultRegistry :: FieldRegistry
defaultRegistry =
  FieldRegistry
    [ -- «Логика»
      SomeFieldSpec lovedSpec
    , SomeFieldSpec ratingSpec
    , SomeFieldSpec averageRatingSpec
    , SomeFieldSpec hasCoverArtSpec
    , SomeFieldSpec compilationSpec
    , -- «История»
      SomeFieldSpec playCountSpec
    , SomeFieldSpec lastPlayedSpec
    , SomeFieldSpec dateAddedSpec
    , SomeFieldSpec dateLovedSpec
    , SomeFieldSpec dateRatedSpec
    , -- «Метаданные»
      SomeFieldSpec titleSpec
    , SomeFieldSpec albumSpec
    , SomeFieldSpec genreSpec
    , SomeFieldSpec yearSpec
    , SomeFieldSpec explicitSpec
    , SomeFieldSpec dateSpec
    , SomeFieldSpec originalYearSpec
    , SomeFieldSpec originalDateSpec
    , SomeFieldSpec releaseYearSpec
    , SomeFieldSpec releaseDateSpec
    , SomeFieldSpec trackNumberSpec
    , SomeFieldSpec discNumberSpec
    , SomeFieldSpec discSubtitleSpec
    , SomeFieldSpec commentSpec
    , SomeFieldSpec lyricsSpec
    , SomeFieldSpec sortTitleSpec
    , SomeFieldSpec sortAlbumSpec
    , SomeFieldSpec sortArtistSpec
    , SomeFieldSpec sortAlbumArtistSpec
    , SomeFieldSpec catalogNumberSpec
    , SomeFieldSpec replayGainSpec
    , SomeFieldSpec replayGainTrackPeakSpec
    , SomeFieldSpec replayGainAlbumGainSpec
    , SomeFieldSpec replayGainAlbumPeakSpec
    , -- «Аудио»
      SomeFieldSpec durationSpec
    , SomeFieldSpec codecSpec
    , SomeFieldSpec bitRateSpec
    , SomeFieldSpec bitDepthSpec
    , SomeFieldSpec sampleRateSpec
    , SomeFieldSpec bpmSpec
    , SomeFieldSpec channelsSpec
    , -- «Файлы»
      SomeFieldSpec filePathSpec
    , SomeFieldSpec fileTypeSpec
    , SomeFieldSpec sizeSpec
    , SomeFieldSpec dateModifiedSpec
    , SomeFieldSpec missingSpec
    , -- «Альбом»
      SomeFieldSpec albumCommentSpec
    , SomeFieldSpec albumRatingSpec
    , SomeFieldSpec albumLovedSpec
    , SomeFieldSpec albumPlayCountSpec
    , SomeFieldSpec albumLastPlayedSpec
    , SomeFieldSpec albumDateLovedSpec
    , SomeFieldSpec albumDateRatedSpec
    , SomeFieldSpec albumDateAddedSpec
    , SomeFieldSpec albumDateModifiedSpec
    , SomeFieldSpec albumDurationSpec
    , SomeFieldSpec albumSongCountSpec
    , SomeFieldSpec albumSizeSpec
    , -- «Артист»
      SomeFieldSpec artistRatingSpec
    , SomeFieldSpec artistLovedSpec
    , SomeFieldSpec artistPlayCountSpec
    , SomeFieldSpec artistLastPlayedSpec
    , SomeFieldSpec artistDateLovedSpec
    , SomeFieldSpec artistDateRatedSpec
    , -- «Идентификаторы»
      SomeFieldSpec mbzAlbumIdSpec
    , SomeFieldSpec mbzAlbumArtistIdSpec
    , SomeFieldSpec mbzArtistIdSpec
    , SomeFieldSpec mbzRecordingIdSpec
    , SomeFieldSpec mbzReleaseTrackIdSpec
    , SomeFieldSpec mbzReleaseGroupIdSpec
    , SomeFieldSpec libraryIdSpec
    , -- «Ссылки»
      SomeFieldSpec playlistRefSpec
    ]

-- | Записи дефолтного реестра в порядке DSL-палитры (вид на
-- 'defaultRegistry' для потребителей, которым нужен список).
fieldSpecs :: [SomeFieldSpec]
fieldSpecs = registrySpecs defaultRegistry

------------------------------------------------------------------------------
-- Проекции спецификации
------------------------------------------------------------------------------

-- | Каноническое имя поля в документации Navidrome.
fieldName :: FieldRef a -> Text
fieldName = spNspName . refSpec

-- | Имя поля в DSL (русские и транслитерированные имена).
--
-- Обратная операция к 'fieldByName': принимает любое имя, которое
-- парсер уже признал для данного поля.
fieldDslName :: FieldRef a -> Text
fieldDslName = spDslName . refSpec

-- | Категория значения поля.
fieldValueType :: FieldRef a -> ValueType
fieldValueType = spCategory . refSpec

-- | Поля, поддерживающие операторы 'Nspeller.Ast.Absent'/'Nspeller.Ast.Present'
-- (ровно там, где документация Navidrome перечисляет поддержку
-- @isMissing@/@isPresent@): часть текстовых полей (включая
-- MusicBrainz ID), числовые поля с пустым значением при отсутствии
-- тега (@bpm@, @bitdepth@) и все поля ReplayGain.
fieldPresence :: FieldRef a -> Bool
fieldPresence = spPresence . refSpec

-- | Закрытый набор значений поля ('Nothing' — значения свободны).
--
-- Ограничение значений проверяется только операторами @=@/@!=@
-- (см. 'Nspeller.Validation'); текстовые операторы
-- (@содержит@ и т. п.) работают со строками как обычно.
fieldEnum :: FieldRef a -> Maybe [EnumVariant]
fieldEnum = spEnum . refSpec

-- | Значения поля целые: дробный операнд ошибка валидации.
--
-- Дробными объявлены только поля, которые Navidrome хранит нецелыми:
-- ReplayGain (@-6.5@), длительности в секундах и средняя оценка.
-- Остальные числовые поля (в том числе годы) — целые.
fieldIsIntegral :: FieldRef a -> Bool
fieldIsIntegral = spIntegral . refSpec

-- | Поле multivalue: одно условие описывает одно из нескольких
-- значений, поэтому два равенства (@жанр = Rock@ и @жанр = Pop@)
-- не противоречат друг другу.
fieldMultivalue :: FieldRef a -> Bool
fieldMultivalue = spMultivalue . refSpec

-- | Ограничения поля или 'Nothing', если область значений неизвестна.
--
-- Задаются только там, где границы действительно известны: рейтинги
-- (трека, альбома, артиста) — 0..5 с шагом 1, счётчики прослушиваний,
-- размеры, частоты, номера и «год» — неотрицательные с шагом 1,
-- длительности — неотрицательные без шага. «оригинальный_год»,
-- «год_релиза», средняя оценка, идентификатор библиотеки и ReplayGain
-- намеренно без границ: пределов для них реестр не знает, и
-- выдумывать их нельзя. Нижняя граница «года» = 0 — не выдумка:
-- отрицательных годов нет в самом домене значений.
fieldNumConstraints :: FieldRef a -> Maybe NumConstraints
fieldNumConstraints = spConstraints . refSpec

-- | Спецификация по имени — DSL, NSP или алиас ('spDslName' /
-- 'spNspName' / 'spAliases'). Единственный поиск по реестру
-- 'defaultRegistry': имена разрешаются через него 'fieldByName' и
-- функции сортировки.
--
-- Как и Navidrome 'LookupField' ('strings.ToLower'), сравнение идёт
-- без учёта регистра (@TITLE@ ≡ @title@ ≡ @Title@): каждая сторона
-- приводится к нижнему регистру. Регистр не влияет на канонический
-- вывод: 'fieldDslName'/'sortFieldName' всегда возвращают
-- 'spDslName'/'spNspName' из реестра, а не ввод пользователя.
specByName :: Text -> Maybe SomeFieldSpec
specByName name = find matches fieldSpecs
  where
    lowName = T.toLower name
    matches (SomeFieldSpec spec) =
      lowName `elem` map T.toLower (specNames spec)

-- | Имя поля из DSL (или NSP, либо алиаса) → ссылка на поле.
--
-- Строится из реестра 'defaultRegistry' — отдельного списка полей
-- нет. Как и в Navidrome, имя сравнивается без учёта регистра
-- ('specByName'). Псевдополя (без возможности 'CapStatic') именем
-- поля не являются: 'fieldByName' для них возвращает 'Nothing'.
fieldByName :: Text -> Maybe SomeField
fieldByName name = case specByName name of
  Just (SomeFieldSpec spec)
    | specHasCapability CapStatic spec -> Just (SomeField (FieldRef spec))
  _ -> Nothing

------------------------------------------------------------------------------
-- Закрытые наборы значений
------------------------------------------------------------------------------

-- | Вариант закрытого набора (enum) значений поля: значение в NSP и
-- подпись для интерфейса. Оба списка ('fieldEnum' для значений,
-- схема @/api/schema@ для подписей) приходят из одного источника —
-- валидация и UI не расходятся.
data EnumVariant = EnumVariant
  { evValue :: Text
  -- ^ Значение операнда (то, что уходит в NSP).
  , evLabel :: Text
  -- ^ Подпись варианта для интерфейса и сообщений об ошибках.
  }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Ограничения числовых полей
------------------------------------------------------------------------------

-- | Известные ограничения числового поля: границы включительно и шаг.
--
-- Единственный источник правды: валидация проверяет по ним выход
-- значений за границы, а UI получает их из @/api/schema@ (ключи
-- @min@/@max@/@step@) для атрибутов числовых полей ввода.
data NumConstraints = NumConstraints
  { ncMin :: Maybe Scientific
  -- ^ Нижняя граница значения включительно.
  , ncMax :: Maybe Scientific
  -- ^ Верхняя граница значения включительно.
  , ncStep :: Maybe Integer
  -- ^ Шаг значения (> 0); 'Nothing' — шаг не известен или не важен.
  }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Поля сортировки
------------------------------------------------------------------------------

-- | Имя поля сортировки (из DSL или уже NSP) → каноническое имя
-- Navidrome. Элемент 'Nspeller.Ast.SortItem' хранит DSL-имя поля, а
-- рендер '.nsp' печатает имя Navidrome — проекция по реестру
-- 'defaultRegistry'. Неизвестное имя возвращается как есть: имена в
-- валидированном AST проверены 'sortFieldByName', запасная ветка
-- недостижима.
sortFieldName :: Text -> Text
sortFieldName name = case specByName name of
  Just (SomeFieldSpec spec) -> spNspName spec
  Nothing -> name

-- | Имя поля из DSL (или NSP, либо алиаса) → каноническое имя
-- сортируемого поля; поле без 'spSortable' и неизвестное имя —
-- 'Nothing'. Поиск без учёта регистра ('specByName'), результат —
-- всегда каноническое 'spDslName' из реестра.
--
-- Сюда входят все поля со 'spSortable': Navidrome сортирует и по
-- логическим полям (@loved@, @hascoverart@), а «подборка»
-- ('spSortable' = 'False') не сортируется.
--
-- Здесь же выполняется канонизация: разобранный '.mix' и DTO
-- принимают и DSL, и NSP имена, а 'Nspeller.Ast.SortItem' работает
-- только с DSL-именем.
sortFieldByName :: Text -> Maybe Text
sortFieldByName name = case specByName name of
  Just (SomeFieldSpec spec)
    | spSortable spec -> Just (spDslName spec)
  _ -> Nothing
