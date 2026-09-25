{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Схема полей и операторов DSL для веб-интерфейса Muzlovar.
--
-- Формируется из реестра спецификаций 'Nspeller.Fields.defaultRegistry':
-- 'fieldSchemas' — проекция каждой 'Nspeller.Fields.FieldSpec' в
-- 'FieldSchema', а списки возможностей (@personalFields@) — проекция
-- 'Nspeller.Fields.capabilityFieldNames'. Фронтенд получает всё это
-- через @GET /api/schema@ и не содержит собственных списков полей —
-- дублирование запрещено контрактом API.
--
-- Соответствие «поле × оператор × значение» совпадает с
-- 'Nspeller.Validation.validatePlaylist' и закреплено тестами
-- (SchemaTests). Группы ингредиентов ('ingredientGroupSchemas')
-- приходят вместе со схемой: фронтенд не хранит собственных списков
-- полей и групп.
--
-- Каталог операторов 'operatorSchemas' (подсказки для палитры) —
-- единственное, что осталось в этом модуле: спецификации поля
-- ссылаются на его идентификаторы.
module Nspeller.Schema
  ( schemaJson
  , FieldSchema (..)
  , fieldSchemas
  , OperatorSchema (..)
  , operatorSchemas
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Scientific (Scientific)
import Data.Text (Text)
import Nspeller.Ast
import Nspeller.Fields

-- | Описание одного оператора DSL.
data OperatorSchema = OperatorSchema
  { osId :: Text
  , osName :: Text
  , osHint :: Text
  }
  deriving (Eq, Show)

-- | Оператор, допустимый для поля: ссылка на каталог
-- 'operatorSchemas' (@id@) плюс категория значения его операнда
-- (@valueType@ — JSON-код 'valueTypeJson': @text@/@number@/@bool@/
-- @date@/@days@/@numberRange@/@dateRange@/@none@). UI по этой
-- метке выбирает редактор значения.
data FieldOp = FieldOp
  { foId :: Text
  , foOperand :: Text
  }
  deriving (Eq, Show)

instance Aeson.ToJSON FieldOp where
  toJSON fo =
    object
      [ "id" .= foId fo
      , "valueType" .= foOperand fo
      ]

-- | Описание одного поля.
data FieldSchema = FieldSchema
  { fsId :: Text
  -- ^ Идентификатор = имя поля в DSL.
  , fsNspName :: Text
  -- ^ Имя поля в JSON Navidrome (.nsp).
  , fsTitle :: Text
  -- ^ Русское название для интерфейса.
  , fsGroup :: Text
  -- ^ Группа ингредиентов в палитре (см. 'ingredientGroupSchemas').
  , fsValueType :: ValueType
  , fsSortable :: Bool
  , fsPresenceCapable :: Bool
  , fsOperators :: [FieldOp]
  -- ^ Операторы, допустимые для поля, с категорией операнда.
  , fsValueVariants :: Maybe [Text]
  -- ^ Закрытый набор значений («да»/«нет») для булевых полей.
  , fsMin :: Maybe Scientific
  -- ^ Нижняя граница числового поля ('Nothing' — не ограничена).
  , fsMax :: Maybe Scientific
  -- ^ Верхняя граница числового поля ('Nothing' — не ограничена).
  , fsStep :: Maybe Integer
  -- ^ Шаг числового поля ('Nothing' — шаг не задан).
  , fsEnum :: Maybe [EnumVariant]
  -- ^ Закрытый набор значений не-булевого поля ('fieldEnum'):
  -- ключи @value@/@label@ в JSON, источник и для валидации, и для UI.
  , fsIntegral :: Bool
  -- ^ Значения поля целые ('fieldIsIntegral'): дробный операнд
  -- ошибка валидации, UI получает @step@/@type@ из этой же метки.
  , fsRefKinds :: Maybe [EnumVariant]
  -- ^ Виды ссылки для типа значения @playlistRef@
  -- ('PlaylistRefType'): допустимые ключи @kind@ со с русскими
  -- подписями. 'Nothing' у обычных полей; UI выбирает вид ссылки
  -- строго по этому списку.
  }
  deriving (Eq, Show)

instance Aeson.ToJSON OperatorSchema where
  toJSON os =
    object
      [ "id" .= osId os
      , "name" .= osName os
      , "hint" .= osHint os
      ]

-- | JSON-код типа значения поля. Отдельная функция, а не @instance ToJSON@,
-- чтобы не создавать orphan-instance для 'ValueType' из "Nspeller.Ast".
valueTypeJson :: ValueType -> Text
valueTypeJson = \case
  TextType -> "text"
  NumberType -> "number"
  BoolType -> "bool"
  DateType -> "date"
  PlaylistRefType -> "playlistRef"

-- | JSON-код одного варианта enum: ручная сборка объекта, чтобы не
-- создавать orphan-instance 'Aeson.ToJSON' для 'EnumVariant' из
-- "Nspeller.Ast".
enumVariantJson :: EnumVariant -> Value
enumVariantJson v =
  object
    [ "value" .= evValue v
    , "label" .= evLabel v
    ]

instance Aeson.ToJSON FieldSchema where
  toJSON fs =
    object
      [ "id" .= fsId fs
      , "nspName" .= fsNspName fs
      , "title" .= fsTitle fs
      , "group" .= fsGroup fs
      , "valueType" .= valueTypeJson (fsValueType fs)
      , "sortable" .= fsSortable fs
      , "presence" .= fsPresenceCapable fs
      , "operators" .= fsOperators fs
      , "valueVariants" .= fsValueVariants fs
      , "min" .= fsMin fs
      , "max" .= fsMax fs
      , "step" .= fsStep fs
      , "enum" .= fmap (map enumVariantJson) (fsEnum fs)
      , "integral" .= fsIntegral fs
      , "refKinds" .= fmap (map enumVariantJson) (fsRefKinds fs)
      ]

------------------------------------------------------------------------------
-- Операторы
------------------------------------------------------------------------------

-- | Полный каталог операторов DSL (для палитры и подсказок).
operatorSchemas :: [OperatorSchema]
operatorSchemas =
  [ OperatorSchema "eq" "=" "равно"
  , OperatorSchema "ne" "!=" "не равно"
  , OperatorSchema "gt" ">" "больше"
  , OperatorSchema "ge" ">=" "больше или равно"
  , OperatorSchema "lt" "<" "меньше"
  , OperatorSchema "le" "<=" "меньше или равно"
  , OperatorSchema "contains" "содержит" "строка содержится в значении"
  , OperatorSchema "notContains" "не содержит" "строка не содержится"
  , OperatorSchema "startsWith" "начинается с" "значение начинается со строки"
  , OperatorSchema "endsWith" "заканчивается на" "значение заканчивается на строку"
  , OperatorSchema "between" "между" "число из диапазона (включительно)"
  , OperatorSchema "inTheLast" "за N дней" "дата попадает в последние N дней"
  , OperatorSchema "notInTheLast" "не звучало N дней" "не прослушивалось N дней"
  , OperatorSchema "before" "до" "дата раньше указанной"
  , OperatorSchema "after" "после" "дата позже указанной"
  , OperatorSchema "isMissing" "отсутствует" "поле не заполнено"
  , OperatorSchema "isPresent" "присутствует" "поле заполнено"
  , OperatorSchema "bare" "флаг" "условие без значения (только для флагов)"
  , OperatorSchema "inPlaylist" "в подборке" "подборка по ссылке id или файлу"
  , OperatorSchema "notInPlaylist" "не в подборке" "не входит в подборку по ссылке"
  ]

------------------------------------------------------------------------------
-- Группы ингредиентов
------------------------------------------------------------------------------

-- | Группы палитры в порядке отображения: @id@ — ключ в 'fsGroup',
-- @name@ — заголовок в интерфейсе. Список полей при этом остаётся
-- единым (порядок DSL-палитры), группировка — только подстановка.
ingredientGroupSchemas :: [(Text, Text)]
ingredientGroupSchemas =
  [ ("logic", "Логика")
  , ("history", "История")
  , ("meta", "Метаданные")
  , ("audio", "Аудио")
  , ("files", "Файлы")
  , ("album", "Альбом")
  , ("artist", "Артист")
  , ("ids", "Идентификаторы")
  , ("links", "Ссылки")
  ]

------------------------------------------------------------------------------
-- Поля
------------------------------------------------------------------------------

-- | Полная таблица полей в порядке DSL-палитры.
--
-- Целиком строится из реестра 'fieldSpecs' (вид на
-- 'Nspeller.Fields.defaultRegistry'): идентификатор, имя в
-- @.nsp@, подпись, группа, категория, операторы, ограничения и
-- признаки берутся из 'FieldSpec' — здесь остаётся только
-- превращение спецификации в описание схемы.
fieldSchemas :: [FieldSchema]
fieldSchemas = [fieldSchemaOf spec | SomeFieldSpec spec <- fieldSpecs]

-- | Спецификация поля → описание поля для @/api/schema@.
fieldSchemaOf :: FieldSpec a -> FieldSchema
fieldSchemaOf spec =
  let nc = spConstraints spec
   in FieldSchema
        { fsId = spDslName spec
        , fsNspName = spNspName spec
        , fsTitle = spTitle spec
        , fsGroup = spGroup spec
        , fsValueType = spCategory spec
        , fsSortable = spSortable spec
        , fsPresenceCapable = spPresence spec
        , fsOperators = map fieldOpSchema (spOperators spec)
        , fsValueVariants = spValueVariants spec
        -- Границы берутся из того же источника, что и валидация
        -- ('spConstraints'): UI и проверка не расходятся.
        , fsMin = ncMin =<< nc
        , fsMax = ncMax =<< nc
        , fsStep = ncStep =<< nc
        , fsEnum = spEnum spec
        , fsIntegral = spIntegral spec
        , fsRefKinds = spRefKinds spec
        }

-- | Оператор спецификации → оператор описания схемы.
fieldOpSchema :: FieldOperator -> FieldOp
fieldOpSchema op = FieldOp (unOpId (fopId op)) (unOperandSlot (fopSlot op))

------------------------------------------------------------------------------
-- Корневой документ
------------------------------------------------------------------------------

-- | JSON-документ @/api/schema@.
schemaJson :: Value
schemaJson =
  object
    [ "version" .= (1 :: Int)
    , "operators" .= operatorSchemas
    , "fields" .= fieldSchemas
    , "sortFields"
        .= [ fsId fs | fs <- fieldSchemas, fsSortable fs ]
    , "groupKinds"
        .= [ object ["id" .= ("all" :: Text), "name" .= ("ВСЕ" :: Text)]
           , object ["id" .= ("any" :: Text), "name" .= ("ЛЮБОЕ" :: Text)]
           ]
    , "ingredientGroups"
        .= [ object ["id" .= gid, "name" .= gname]
           | (gid, gname) <- ingredientGroupSchemas
           ]
    , "sortDirections"
        .= [ object ["id" .= ("asc" :: Text), "name" .= ("по возрастанию" :: Text)]
           , object ["id" .= ("desc" :: Text), "name" .= ("по убыванию" :: Text)]
           ]
    , "personalFields" .= capabilityFieldNames CapPersonal
    ]
