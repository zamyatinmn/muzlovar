{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Схема полей и операторов DSL для веб-интерфейса Muzlovar.
--
-- Единственный источник метаданных: строится из GADT
-- 'Nspeller.Ast.Field', 'Nspeller.Ast.PresenceField' и
-- 'Nspeller.Ast.SortField'. Фронтенд получает её через
-- @GET /api/schema@ и не содержит собственных списков полей —
-- дублирование запрещено контрактом API.
--
-- Соответствие «поле × оператор × значение» совпадает с
-- 'Nspeller.Validation.validatePlaylist' и закреплено тестами
-- (SchemaTests).
module Nspeller.Schema
  ( schemaJson
  , FieldSchema (..)
  , fieldSchemas
  , OperatorSchema (..)
  , operatorSchemas
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import Nspeller.Ast

-- | Описание одного оператора DSL.
data OperatorSchema = OperatorSchema
  { osId :: Text
  , osName :: Text
  , osHint :: Text
  }
  deriving (Eq, Show)

-- | Описание одного поля.
data FieldSchema = FieldSchema
  { fsId :: Text
  -- ^ Идентификатор = имя поля в DSL.
  , fsNspName :: Text
  -- ^ Имя поля в JSON Navidrome (.nsp).
  , fsTitle :: Text
  -- ^ Русское название для интерфейса.
  , fsValueType :: ValueType
  , fsSortable :: Bool
  , fsPresenceCapable :: Bool
  , fsOperators :: [Text]
  -- ^ Идентификаторы операторов, допустимых для поля.
  , fsValueVariants :: Maybe [Text]
  -- ^ Закрытый набор значений («да»/«нет») для булевых полей.
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

instance Aeson.ToJSON FieldSchema where
  toJSON fs =
    object
      [ "id" .= fsId fs
      , "nspName" .= fsNspName fs
      , "title" .= fsTitle fs
      , "valueType" .= valueTypeJson (fsValueType fs)
      , "sortable" .= fsSortable fs
      , "presence" .= fsPresenceCapable fs
      , "operators" .= fsOperators fs
      , "valueVariants" .= fsValueVariants fs
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
  , OperatorSchema "lt" "<" "меньше"
  , OperatorSchema "contains" "содержит" "строка содержится в значении"
  , OperatorSchema "notContains" "не содержит" "строка не содержится"
  , OperatorSchema "startsWith" "начинается с" "значение начинается со строки"
  , OperatorSchema "endsWith" "заканчивается на" "значение заканчивается на строку"
  , OperatorSchema "between" "между" "число из диапазона (включительно)"
  , OperatorSchema "inTheLast" "за N дней" "дата попадает в последние N дней"
  , OperatorSchema "notInTheLast" "не звучало N дней" "не прослушивалось N дней"
  , OperatorSchema "isMissing" "отсутствует" "поле не заполнено"
  , OperatorSchema "isPresent" "присутствует" "поле заполнено"
  , OperatorSchema "bare" "флаг" "условие без значения (только для флагов)"
  ]

------------------------------------------------------------------------------
-- Поля
------------------------------------------------------------------------------

-- | Идентификаторы операторов для каждого типа поля.
textOps :: [Text]
textOps = ["eq", "ne", "contains", "notContains", "startsWith", "endsWith"]

numberOps :: [Text]
numberOps = ["eq", "ne", "gt", "lt", "between"]

boolOps :: [Text]
boolOps = ["eq", "ne", "bare"]

-- | Операторы дат: сравнение в днях; «не звучало» только для
-- последнего прослушивания (как в 'Nspeller.Validation').
dateOps :: Field a -> [Text]
dateOps LastPlayed = ["inTheLast", "notInTheLast"]
dateOps _ = ["inTheLast"]

-- | Полная таблица полей в порядке DSL-палитры.
--
-- Идентификатор, имя в @.nsp@, тип и набор операторов берутся из GADT
-- 'Field' — здесь заданы только русские названия для интерфейса.
fieldSchemas :: [FieldSchema]
fieldSchemas =
  [ flag Loved "Любимое"
  , num Rating "Оценка"
  , num PlayCount "Прослушивания"
  , date LastPlayed "Последнее прослушивание"
  , date DateAdded "Добавлено"
  , txt Title "Название"
  , txt Album "Альбом"
  , txt Genre "Жанр"
  , num Year "Год"
  , txt ExplicitStatus "Explicit"
  , flag HasCoverArt "Обложка"
  , num RGTrackGain "ReplayGain"
  ]
  where
    mk f title ops presence variants =
      FieldSchema
        { fsId = fieldDslName f
        , fsNspName = fieldName f
        , fsTitle = title
        , fsValueType = fieldValueType f
        , fsSortable = isSortableField f
        , fsPresenceCapable = presence
        , fsOperators = ops
        , fsValueVariants = variants
        }
    txt f title = mk f title textOps (isJust (fieldPresence f)) Nothing
    num f title = mk f title numberOps (isJust (fieldPresence f)) Nothing
    flag f title =
      mk f title boolOps False (Just ["да", "нет"])
    date f title = mk f title (dateOps f) False Nothing

-- | Поле участвует в сортировке (все, кроме булевых) — по тому же
-- признаку, что и 'Nspeller.Ast.sortFieldByName'.
isSortableField :: Field a -> Bool
isSortableField = \case
  Loved -> False
  HasCoverArt -> False
  _ -> True

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
    , "sortDirections"
        .= [ object ["id" .= ("asc" :: Text), "name" .= ("по возрастанию" :: Text)]
           , object ["id" .= ("desc" :: Text), "name" .= ("по убыванию" :: Text)]
           ]
    , "personalFields"
        .= [ fieldDslName Loved
           , fieldDslName Rating
           , fieldDslName PlayCount
           , fieldDslName LastPlayed
           ]
    ]
