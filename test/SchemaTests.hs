{-# LANGUAGE OverloadedStrings #-}

-- | Тесты @/api/schema@: структура JSON-документа и перекрёстная
-- проверка «схема ⟺ валидация ядра» — для каждого поля и оператора
-- 'compilePlaylistDto' успешен тогда и только тогда, когда оператор
-- разрешён схемой.
module SchemaTests (schemaTests) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isRight)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Nspeller.Ast (ValueType (..))
import Nspeller.Muzlovar.Types
import Nspeller.Schema (schemaJson)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit

------------------------------------------------------------------------------
-- Доступ к JSON-документу
------------------------------------------------------------------------------

-- | Массив значений по ключу (пустой, если ключа или массива нет).
arrayAt :: Key -> Value -> [Value]
arrayAt k (Object o) = case KM.lookup k o of
  Just (Array v) -> foldr (:) [] v
  _ -> []
arrayAt _ _ = []

-- | Текстовое поле объекта-значения (пусто, если нет).
textAt :: Key -> Value -> Text
textAt k (Object o) = case KM.lookup k o of
  Just (String t) -> t
  _ -> ""
textAt _ _ = ""

-- | Массив строк по ключу.
textListAt :: Key -> Value -> [Text]
textListAt k (Object o) = case KM.lookup k o of
  Just (Array v) -> [t | String t <- foldr (:) [] v]
  _ -> []
textListAt _ _ = []

-- | Булево поле объекта-значения.
boolAt :: Key -> Value -> Bool
boolAt k (Object o) = case KM.lookup k o of
  Just (Bool b) -> b
  _ -> False
boolAt _ _ = False

-- | Закрытый набор значений поля (или 'Nothing').
variantsAt :: Value -> Maybe [Text]
variantsAt (Object o) = case KM.lookup "valueVariants" o of
  Just Null -> Nothing
  Just (Array v) -> Just [t | String t <- foldr (:) [] v]
  _ -> Nothing
variantsAt _ = Nothing

-- | Категория значения поля из JSON.
valueTypeOf :: Value -> ValueType
valueTypeOf f = case textAt "valueType" f of
  "text" -> TextType
  "number" -> NumberType
  "bool" -> BoolType
  _ -> DateType

------------------------------------------------------------------------------
-- Структура схемы
------------------------------------------------------------------------------

-- | Ожидаемые идентификаторы 12 полей DSL.
dslFieldIds :: [Text]
dslFieldIds =
  [ "название"
  , "альбом"
  , "жанр"
  , "explicit"
  , "год"
  , "оценка"
  , "прослушиваний"
  , "replaygain"
  , "любимое"
  , "обложка"
  , "последнее_прослушивание"
  , "добавлено"
  ]

-- | Ожидаемые идентификаторы 14 операторов.
dslOperatorIds :: [Text]
dslOperatorIds =
  [ "eq"
  , "ne"
  , "contains"
  , "notContains"
  , "startsWith"
  , "endsWith"
  , "gt"
  , "lt"
  , "between"
  , "inTheLast"
  , "notInTheLast"
  , "isMissing"
  , "isPresent"
  , "bare"
  ]

schemaStructure :: TestTree
schemaStructure = testCase "структура /api/schema" $ case schemaJson of
  Object o -> do
    KM.lookup "version" o @?= Just (Number 1)
    let fields = arrayAt "fields" schemaJson
        ops = arrayAt "operators" schemaJson
        ids = map (textAt "id") fields
        opIds = map (textAt "id") ops
        sortFields = textListAt "sortFields" schemaJson
        personal = textListAt "personalFields" schemaJson
    -- поля и операторы
    length fields @?= 12
    length ops @?= 14
    length (nub ids) @?= 12
    length (nub opIds) @?= 14
    sort ids @?= sort dslFieldIds
    sort opIds @?= sort dslOperatorIds
    -- группы и направления сортировки
    map (textAt "id") (arrayAt "groupKinds" schemaJson) @?= ["all", "any"]
    map (textAt "id") (arrayAt "sortDirections" schemaJson) @?= ["asc", "desc"]
    -- персональные поля
    personal @?= ["любимое", "оценка", "прослушиваний", "последнее_прослушивание"]
    assertBool
      ("персональные поля не входят в список полей: " <> show personal)
      (all (`elem` ids) personal)
    -- поля сортировки = поля с флагом sortable
    length sortFields @?= 10
    sortFields @?= [textAt "id" f | f <- fields, boolAt "sortable" f]
    assertBool "булево поле входит в сортировку" (all (`notElem` ["любимое", "обложка"]) sortFields)
    -- закрытый набор значений только у булевых полей
    forM_ fields $ \f ->
      variantsAt f
        @?= (if valueTypeOf f == BoolType then Just ["да", "нет"] else Nothing)
    -- названия полей заполнены
    forM_ fields $ \f ->
      assertBool
        ("пустое название поля " <> T.unpack (textAt "id" f))
        (not (T.null (textAt "title" f)))
  _ -> assertFailure "schemaJson не является JSON-объектом"

------------------------------------------------------------------------------
-- Перекрёстная проверка «схема ⟺ валидация»
------------------------------------------------------------------------------

-- | Значение-кандидат для проверки «поле × оператор»: нейтральное,
-- чтобы не пройти валидацию из-под значения, когда оператор
-- совместим с полем.
probeValue :: ValueType -> Text -> Maybe Value
probeValue vt op = case op of
  "bare" -> Nothing
  "isMissing" -> Nothing
  "isPresent" -> Nothing
  "between" -> Just (toJSON ([1, 2] :: [Integer]))
  "inTheLast" -> Just (Number 7)
  "notInTheLast" -> Just (Number 7)
  "gt" -> Just (Number 1)
  "lt" -> Just (Number 1)
  "eq" -> eqValue
  "ne" -> eqValue
  _ -> Just (String "икра")
  where
    eqValue = case vt of
      TextType -> Just (String "икра")
      NumberType -> Just (Number 1)
      BoolType -> Just (Bool False)
      DateType -> Just (Number 1)

-- | Подборка ровно с одним условием.
probeDto :: Text -> Text -> Maybe Value -> PlaylistDto
probeDto fld op mv =
  PlaylistDto
    "Проверка"
    Nothing
    False
    (GroupDto "all" [ItemCond (CondDto fld op mv)])
    Nothing
    Nothing

schemaValidationAgreement :: TestTree
schemaValidationAgreement = testCase "операторы схемы совпадают с валидацией" $ do
  let fields = arrayAt "fields" schemaJson
      opIds = map (textAt "id") (arrayAt "operators" schemaJson)
  length fields @?= 12
  length opIds @?= 14
  forM_ fields $ \f -> do
    let fid = textAt "id" f
        ops = textListAt "operators" f
        vt = valueTypeOf f
        pres = boolAt "presence" f
    forM_ opIds $ \op -> do
      let expected = op `elem` ops || (pres && op `elem` ["isMissing", "isPresent"])
          actual = isRight (compilePlaylistDto (probeDto fid op (probeValue vt op)))
      assertEqual (T.unpack (fid <> " × " <> op)) expected actual

------------------------------------------------------------------------------
-- Сообщения об ошибках
------------------------------------------------------------------------------

-- | Компиляция должна упасть с фрагментом сообщения.
msgTest :: TestName -> PlaylistDto -> Text -> TestTree
msgTest name dto needle = testCase name $ case compilePlaylistDto dto of
  Right _ -> assertFailure "ожидалась ошибка компиляции"
  Left [] -> assertFailure "список ошибок пуст"
  Left es ->
    assertBool
      ("нет фрагмента «" <> T.unpack needle <> "»; получено: " <> show (map aeMessage es))
      (any (T.isInfixOf needle . aeMessage) es)

-- | Компиляция должна упасть с данным кодом ошибки.
codeTest :: TestName -> PlaylistDto -> Text -> TestTree
codeTest name dto code = testCase name $ case compilePlaylistDto dto of
  Right _ -> assertFailure "ожидалась ошибка компиляции"
  Left [] -> assertFailure "список ошибок пуст"
  Left (e : _) -> aeCode e @?= code

validationMessages :: TestTree
validationMessages =
  testGroup
    "Сообщения валидации из DTO"
    [ msgTest
        "numOnlyErr: числовой оператор на текстовом поле"
        (probeDto "название" "gt" (Just (Number 1)))
        "Оператор «>» применим только к числовым полям."
    , msgTest
        "textOnlyErr: текстовый оператор на числовом поле"
        (probeDto "год" "contains" (Just (String "икра")))
        "Оператор «содержит» применим только к текстовым полям."
    , msgTest
        "presenceErr: проверка наличия неполе"
        (probeDto "название" "isMissing" Nothing)
        "не поддерживает проверку наличия"
    , msgTest
        "rangeErr: перевёрнутый диапазон"
        (probeDto "год" "between" (Just (toJSON ([1990, 1980] :: [Integer]))))
        "Нижняя граница диапазона не может быть больше верхней"
    , msgTest
        "неположительное число дней"
        (probeDto "добавлено" "inTheLast" (Just (Number 0)))
        "положительным"
    , msgTest
        "dateOnlyErr: за N дней на текстовом поле"
        (probeDto "жанр" "inTheLast" (Just (Number 7)))
        "Сравнение «за … дней» применимо только к датовым полям"
    , codeTest
        "неизвестное поле -> validation"
        (probeDto "меме" "gt" (Just (Number 1)))
        "validation"
    , codeTest
        "структурная ошибка -> invalid_tree"
        ((probeDto "название" "eq" (Just (String "x"))) {pdName = ""})
        "invalid_tree"
    , codeTest
        "notInTheLast не для последнего прослушивания -> invalid_tree"
        (probeDto "добавлено" "notInTheLast" (Just (Number 7)))
        "invalid_tree"
    ]

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

schemaTests :: TestTree
schemaTests =
  testGroup
    "Schema"
    [ schemaStructure
    , schemaValidationAgreement
    , validationMessages
    ]
