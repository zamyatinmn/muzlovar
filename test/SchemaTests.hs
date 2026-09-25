{-# LANGUAGE OverloadedStrings #-}

-- | Тесты @/api/schema@: структура JSON-документа и перекрёстная
-- проверка «схема ⟺ валидация ядра» — для каждого поля и оператора
-- 'compilePlaylistDto' успешен тогда и только тогда, когда оператор
-- разрешён схемой.
module SchemaTests (schemaTests) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, toJSON, (.=))
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

-- | Закрытой набор (enum) поля из JSON: пары (значение, подпись)
-- или 'Nothing', если ключа/массива нет.
enumAt :: Key -> Value -> Maybe [(Text, Text)]
enumAt k (Object o) = case KM.lookup k o of
  Just (Array v) -> Just [(textAt "value" x, textAt "label" x) | x <- foldr (:) [] v]
  _ -> Nothing
enumAt _ _ = Nothing

-- | Категория значения поля из JSON.
valueTypeOf :: Value -> ValueType
valueTypeOf f = case textAt "valueType" f of
  "text" -> TextType
  "number" -> NumberType
  "bool" -> BoolType
  "playlistRef" -> PlaylistRefType
  _ -> DateType

------------------------------------------------------------------------------
-- Структура схемы
------------------------------------------------------------------------------

-- | Ожидаемые идентификаторы 72 полей DSL (в порядке палитры:
-- «Логика», «История», «Метаданные», «Аудио», «Файлы», «Альбом»,
-- «Артист», «Идентификаторы», «Ссылки»).
dslFieldIds :: [Text]
dslFieldIds =
  [ -- «Логика»
    "любимое"
  , "оценка"
  , "средняя_оценка"
  , "обложка"
  , "сборник"
  , -- «История»
    "прослушиваний"
  , "последнее_прослушивание"
  , "добавлено"
  , "дата_любимого"
  , "дата_оценки"
  , -- «Метаданные»
    "название"
  , "альбом"
  , "жанр"
  , "год"
  , "explicit"
  , "дата_записи"
  , "оригинальный_год"
  , "оригинальная_дата"
  , "год_релиза"
  , "дата_релиза"
  , "номер_трека"
  , "номер_диска"
  , "подзаголовок_диска"
  , "комментарий"
  , "текст_песни"
  , "сортировка_названия"
  , "сортировка_альбома"
  , "сортировка_артиста"
  , "сортировка_альбомного_артиста"
  , "номер_в_каталоге"
  , "replaygain"
  , "replaygain_пик"
  , "replaygain_альбом"
  , "replaygain_пик_альбом"
  , -- «Аудио»
    "длительность"
  , "кодек"
  , "битрейт"
  , "битовая_глубина"
  , "частота_дискретизации"
  , "темп"
  , "каналы"
  , -- «Файлы»
    "путь_к_файлу"
  , "тип_файла"
  , "размер"
  , "изменено"
  , "файл_отсутствует"
  , -- «Альбом»
    "комментарий_альбома"
  , "оценка_альбома"
  , "любимый_альбом"
  , "прослушиваний_альбома"
  , "последнее_прослушивание_альбома"
  , "дата_любимого_альбома"
  , "дата_оценки_альбома"
  , "дата_добавления_альбома"
  , "дата_изменения_альбома"
  , "длительность_альбома"
  , "треков_в_альбоме"
  , "размер_альбома"
  , -- «Артист»
    "оценка_артиста"
  , "любимый_артист"
  , "прослушиваний_артиста"
  , "последнее_прослушивание_артиста"
  , "дата_любимого_артиста"
  , "дата_оценки_артиста"
  , -- «Идентификаторы»
    "mbid_альбома"
  , "mbid_альбомного_артиста"
  , "mbid_артиста"
  , "mbid_записи"
  , "mbid_трека_релиза"
  , "mbid_группы_релизов"
  , "библиотека"
  , -- «Ссылки»
    "подборка"
  ]

-- | Персональные поля в порядке реестра (18 штук).
personalFieldIds :: [Text]
personalFieldIds =
  [ "любимое"
  , "оценка"
  , "прослушиваний"
  , "последнее_прослушивание"
  , "дата_любимого"
  , "дата_оценки"
  , "оценка_альбома"
  , "любимый_альбом"
  , "прослушиваний_альбома"
  , "последнее_прослушивание_альбома"
  , "дата_любимого_альбома"
  , "дата_оценки_альбома"
  , "оценка_артиста"
  , "любимый_артист"
  , "прослушиваний_артиста"
  , "последнее_прослушивание_артиста"
  , "дата_любимого_артиста"
  , "дата_оценки_артиста"
  ]

-- | Поля, значения которых Navidrome хранит дробными (неполные
-- 'fsIntegral'): ReplayGain, длительности и средняя оценка.
nonIntegralFieldIds :: [Text]
nonIntegralFieldIds =
  [ "replaygain"
  , "replaygain_пик"
  , "replaygain_альбом"
  , "replaygain_пик_альбом"
  , "длительность"
  , "длительность_альбома"
  , "средняя_оценка"
  ]

-- | Ожидаемые идентификаторы 20 операторов.
dslOperatorIds :: [Text]
dslOperatorIds =
  [ "eq"
  , "ne"
  , "contains"
  , "notContains"
  , "startsWith"
  , "endsWith"
  , "gt"
  , "ge"
  , "lt"
  , "le"
  , "between"
  , "inTheLast"
  , "notInTheLast"
  , "before"
  , "after"
  , "isMissing"
  , "isPresent"
  , "bare"
  , "inPlaylist"
  , "notInPlaylist"
  ]

-- | JSON-объект поля по идентификатору ('Data.Aeson.Null', если поле
-- не найдено — тесты сверки упадут на несовпадении).
fieldObj :: [Value] -> Text -> Value
fieldObj fields fid = case [f | f <- fields, textAt "id" f == fid] of
  (f : _) -> f
  [] -> Null

-- | Значение ключа объекта-значения ('Data.Aeson.Null', если ключа
-- нет или это не объект).
fieldKey :: Key -> Value -> Value
fieldKey k (Object o) = case KM.lookup k o of
  Just v -> v
  Nothing -> Null
fieldKey _ _ = Null

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
    length fields @?= 72
    length ops @?= 20
    length (nub ids) @?= 72
    length (nub opIds) @?= 20
    sort ids @?= sort dslFieldIds
    sort opIds @?= sort dslOperatorIds
    -- группы и направления сортировки
    map (textAt "id") (arrayAt "groupKinds" schemaJson) @?= ["all", "any"]
    map (textAt "id") (arrayAt "sortDirections" schemaJson) @?= ["asc", "desc"]
    -- группы ингредиентов палитры: имя, порядок и принадлежность полей
    let ingredientIds = map (textAt "id") (arrayAt "ingredientGroups" schemaJson)
    ingredientIds
      @?= ["logic", "history", "meta", "audio", "files", "album", "artist", "ids", "links"]
    map (textAt "name") (arrayAt "ingredientGroups" schemaJson)
      @?= ["Логика", "История", "Метаданные", "Аудио", "Файлы", "Альбом", "Артист", "Идентификаторы", "Ссылки"]
    forM_ fields $ \f ->
      assertBool
        ("поле " <> T.unpack (textAt "id" f) <> " не отнесено к группе ингредиентов")
        (textAt "group" f `elem` ingredientIds)
    -- персональные поля
    personal @?= personalFieldIds
    assertBool
      ("персональные поля не входят в список полей: " <> show personal)
      (all (`elem` ids) personal)
    -- поля сортировки = поля с флагом sortable (все, кроме «подборка»)
    length sortFields @?= 71
    sortFields @?= [textAt "id" f | f <- fields, boolAt "sortable" f]
    assertBool
      "логические поля не входят в сортировку"
      (all (`elem` sortFields) ["любимое", "обложка"])
    assertBool "псевдополе «подборка» не сортируется" ("подборка" `notElem` sortFields)
    -- закрытый набор значений только у булевых полей; enum — только
    -- у explicit (значения e/c/"" с подписями)
    forM_ fields $ \f -> do
      variantsAt f
        @?= (if valueTypeOf f == BoolType then Just ["да", "нет"] else Nothing)
      enumAt "enum" f
        @?= ( if textAt "id" f == "explicit"
                then Just [("e", "Explicit"), ("c", "Clean"), ("", "Не определено")]
                else Nothing
            )
    -- признак целостности чисел: дробными объявлены ровно ReplayGain,
    -- длительности и средняя оценка
    forM_ fields $ \f ->
      boolAt "integral" f @?= (textAt "id" f `notElem` nonIntegralFieldIds)
    -- виды ссылки: только у поля подборка (id + путь к файлу)
    forM_ fields $ \f ->
      enumAt "refKinds" f
        @?= ( if textAt "id" f == "подборка"
                then Just [("id", "ID"), ("path", "путь к файлу")]
                else Nothing
            )
    -- ограничения числовых полей совпадают с валидацией ядра:
    -- рейтинги 0..5 шаг 1, счётчики/размеры/год ≥ 0 шаг 1, длительности
    -- ≥ 0 без шага, остальные годы/идентификаторы без границ
    fieldKey "min" (fieldObj fields "оценка") @?= toJSON (0 :: Integer)
    fieldKey "max" (fieldObj fields "оценка") @?= toJSON (5 :: Integer)
    fieldKey "step" (fieldObj fields "оценка") @?= toJSON (1 :: Integer)
    fieldKey "min" (fieldObj fields "оценка_альбома") @?= toJSON (0 :: Integer)
    fieldKey "max" (fieldObj fields "оценка_альбома") @?= toJSON (5 :: Integer)
    fieldKey "step" (fieldObj fields "оценка_альбома") @?= toJSON (1 :: Integer)
    fieldKey "min" (fieldObj fields "прослушиваний") @?= toJSON (0 :: Integer)
    fieldKey "max" (fieldObj fields "прослушиваний") @?= Null
    fieldKey "step" (fieldObj fields "прослушиваний") @?= toJSON (1 :: Integer)
    fieldKey "min" (fieldObj fields "размер") @?= toJSON (0 :: Integer)
    fieldKey "step" (fieldObj fields "размер") @?= toJSON (1 :: Integer)
    fieldKey "min" (fieldObj fields "длительность") @?= toJSON (0 :: Integer)
    fieldKey "step" (fieldObj fields "длительность") @?= Null
    -- «Год»: min=0, step=1, max не задан (см. 'yearConstraints')
    fieldKey "min" (fieldObj fields "библиотека") @?= Null
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
-- совместим с полем. Для поля с enum-набором равенство берёт
-- допустимое значение (см. 'fieldEnum'); для датового поля —
-- дату @ГГГГ-ММ-ДД@ (и пару дат у диапазона).
probeValue :: Text -> ValueType -> Text -> Maybe Value
probeValue fid vt op = case op of
  "bare" -> Nothing
  "isMissing" -> Nothing
  "isPresent" -> Nothing
  "between" -> betweenValue
  "inTheLast" -> Just (Number 7)
  "notInTheLast" -> Just (Number 7)
  "gt" -> cmpValue
  "ge" -> cmpValue
  "lt" -> cmpValue
  "le" -> cmpValue
  "before" -> cmpValue
  "after" -> cmpValue
  "eq" -> eqValue
  "ne" -> eqValue
  "inPlaylist" -> refValue
  "notInPlaylist" -> refValue
  _ -> Just (String "икра")
  where
    -- Операнд сравнения: дата для датового поля, число — иначе.
    cmpValue = case vt of
      DateType -> Just (String "2020-01-01")
      _ -> Just (Number 1)
    betweenValue = case vt of
      DateType -> Just (toJSON [String "2019-01-01", String "2019-12-31"])
      _ -> Just (toJSON ([1, 2] :: [Integer]))
    eqValue = case vt of
      TextType
        | fid == "explicit" -> Just (String "e")
        | otherwise -> Just (String "икра")
      NumberType -> Just (Number 1)
      BoolType -> Just (Bool False)
      DateType -> Just (String "2020-01-01")
      PlaylistRefType -> refValue
    -- Ссылка на подборку: операнд inPlaylist/notInPlaylist.
    refValue = Just (object ["kind" .= ("id" :: Text), "value" .= ("abc-123" :: Text)])

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
  length fields @?= 72
  length opIds @?= 20
  forM_ fields $ \f -> do
    let fid = textAt "id" f
        -- операторы поля — объекты @{id, valueType}@ (см. 'FieldOp')
        ops = map (textAt "id") (arrayAt "operators" f)
        vt = valueTypeOf f
        pres = boolAt "presence" f
    forM_ opIds $ \op -> do
      let expected = op `elem` ops || (pres && op `elem` ["isMissing", "isPresent"])
          actual = isRight (compilePlaylistDto (probeDto fid op (probeValue fid vt op)))
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
    , msgTest
        "enumErr: значение вне набора explicit"
        (probeDto "explicit" "eq" (Just (String "икра")))
        "не входит в допустимые значения"
    , msgTest
        "enumErr: перечень допустимых значений"
        (probeDto "explicit" "eq" (Just (String "икра")))
        "Допустимо: «e» (Explicit), «c» (Clean), «» (Не определено)."
    , msgTest
        "fractionalErr: дробное значение целочисленного поля"
        (probeDto "год" "gt" (Just (Number 1.5)))
        "Значение 1.5 должно быть целым числом."
    , msgTest
        "fractionalErr: поле не поддерживает дробные значения"
        (probeDto "год" "between" (Just (toJSON [Number 1980, Number 1980.5])))
        "не поддерживает дробные значения"
    , codeTest
        "notInTheLast не для датового поля -> invalid_tree"
        (probeDto "жанр" "notInTheLast" (Just (Number 7)))
        "invalid_tree"
    ]

-- | Компиляция DTO должна пройти (путь редактора: POST /api/validate).
okTest :: TestName -> PlaylistDto -> TestTree
okTest name dto = testCase name $ case compilePlaylistDto dto of
  Right _ -> pure ()
  Left es ->
    assertFailure ("ожидался успех компиляции, получено: " <> show (map aeMessage es))

-- | Regression: отрицательный год отклоняется ядром независимо от UI.
--
-- Цепочка одна на всех: реестр ('Nspeller.Fields.yearSpec', min=0,
-- step=1) → 'fieldNumConstraints' → валидация и @/api/schema@, откуда
-- min/step уходят в атрибуты input и clamp редактора. Здесь проверяется
-- серверная половина: DTO с @год = -1@ не компилируется, @год = 0@ —
-- граничное допустимое значение, а схема отдаёт UI ровно min=0/step=1.
yearConstraints :: TestTree
yearConstraints =
  testGroup
    "Ограничения поля «Год»"
    [ msgTest
        "год = -1 → ошибка валидации"
        (probeDto "год" "eq" (Just (Number (-1))))
        "вне допустимого диапазона поля «год»: не меньше 0"
    , msgTest
        "год < 0 → условие не может выполняться"
        (probeDto "год" "lt" (Just (Number 0)))
        "Условие не может выполняться"
    , msgTest
        "год между -5 и 10 → диапазон выходит за границы"
        (probeDto "год" "between" (Just (toJSON [-5 :: Integer, 10])))
        "выходит за допустимые границы"
    , okTest
        "год = 0 → допустим"
        (probeDto "год" "eq" (Just (Number 0)))
    , testCase "schema: UI получает min=0 и step=1 без max" $ do
        let f = fieldObj (arrayAt "fields" schemaJson) "год"
        fieldKey "min" f @?= toJSON (0 :: Integer)
        fieldKey "step" f @?= toJSON (1 :: Integer)
        fieldKey "max" f @?= Null
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
    , yearConstraints
    ]
