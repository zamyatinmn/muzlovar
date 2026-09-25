{-# LANGUAGE OverloadedStrings #-}

-- | Тесты DTO веб-редактора Muzlovar: JSON-контракт 'PlaylistDto',
-- структурные проверки дерева ('dtoToParsed'), конвейер
-- 'compilePlaylistDto' и обратные переводы @.mix@/@.nsp@ -> DTO.
module DtoTests (dtoTests) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nspeller.Muzlovar.Store (containsRawDto)
import Nspeller.Muzlovar.Types
import System.FilePath ((</>), (<.>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit

------------------------------------------------------------------------------
-- Вспомогательные функции
------------------------------------------------------------------------------

-- | Каталог golden-файлов ядра.
goldenDir :: FilePath
goldenDir = "test" </> "golden"

-- | Содержимое golden @.mix@ в тексте.
goldenSrc :: String -> IO Text
goldenSrc name = do
  bs <- BS.readFile (goldenDir </> name <.> "mix")
  case TE.decodeUtf8' bs of
    Left _ -> assertFailure ("golden-файл не в UTF-8: " <> name)
    Right t -> pure t

-- | Группа «все» с одним условием.
oneCond :: CondDto -> GroupDto
oneCond c = GroupDto "all" [ItemCond c]

-- | Валидное условие-заготовка.
plainCond :: CondDto
plainCond = CondDto "название" "eq" (Just (String "Тест"))

-- | Минимальная подборка: имя и одно условие.
simpleDto :: Text -> PlaylistDto
simpleDto nm = PlaylistDto nm Nothing False (oneCond plainCond) Nothing Nothing

-- | Подборка только с корнем (для проверки структуры).
baseDto :: GroupDto -> PlaylistDto
baseDto g = PlaylistDto "Тест" Nothing False g Nothing Nothing

-- | Цепочка из @n@ вложенных групп с условием в глубине:
-- 'nesting' корня равен ровно @n@.
nestedGroups :: Int -> GroupDto
nestedGroups 0 = GroupDto "all" [ItemCond plainCond]
nestedGroups n = GroupDto "all" [ItemGroup (nestedGroups (n - 1))]

-- | Разбор @.nsp@-литерала в DTO (для тестов обратного перевода).
nspFrom :: LBS.ByteString -> Either Text PlaylistDto
nspFrom bs = case (eitherDecode bs :: Either String Value) of
  Left e -> Left (T.pack e)
  Right v -> nspToDto v

-- | 'dtoToParsed' обязан вернуть ошибку с данным путём и фрагментом
-- сообщения.
assertDtoError :: TestName -> PlaylistDto -> Text -> Text -> TestTree
assertDtoError name dto path needle = testCase name $
  case dtoToParsed dto of
    Right _ -> assertFailure "ожидалась структурная ошибка, но DTO разобрался"
    Left es ->
      case [e | e <- es, dePath e == path, needle `T.isInfixOf` deMessage e] of
        [] ->
          assertFailure
            ( "нет ошибки {путь "
                <> T.unpack path
                <> ", сообщение содержит «"
                <> T.unpack needle
                <> "»}; получено: "
                <> show es
            )
        _ -> pure ()

-- | Компиляция DTO должна завершиться ошибкой валидации с фрагментом
-- сообщения.
assertCompileError :: TestName -> PlaylistDto -> Text -> TestTree
assertCompileError name dto needle = testCase name $
  case compilePlaylistDto dto of
    Right _ -> assertFailure "ожидалась ошибка компиляции"
    Left [] -> assertFailure "список ошибок пуст"
    Left es ->
      assertBool
        ("нет фрагмента «" <> T.unpack needle <> "»; получено: " <> show (map aeMessage es))
        (any (\e -> needle `T.isInfixOf` aeMessage e) es)

-- | Компиляция успешна, и .nsp содержит все перечисленные фрагменты.
assertNspContains :: TestName -> PlaylistDto -> [Text] -> TestTree
assertNspContains name dto needles = testCase name $
  case compilePlaylistDto dto of
    Left es -> assertFailure ("ошибки компиляции: " <> show es)
    Right c -> do
      let nsp = TE.decodeUtf8 (LBS.toStrict (cmpNsp c))
      forM_ needles $ \needle ->
        assertBool
          ("нет «" <> T.unpack needle <> "» в .nsp: " <> T.unpack nsp)
          (needle `T.isInfixOf` nsp)

------------------------------------------------------------------------------
-- JSON-контракт
------------------------------------------------------------------------------

-- | Заготовка для проверок JSON и конвейера: описание, публичность,
-- вложенные группы, сортировка по полям и лимит.
validDto :: PlaylistDto
validDto =
  PlaylistDto
    { pdName = "Тестовая подборка"
    , pdDescription = Just "Описание подборки"
    , pdPublic = True
    , pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "год" "between" (Just (toJSON ([1980, 1989] :: [Integer]))))
          , ItemCond (CondDto "жанр" "contains" (Just (String "rock")))
          , ItemGroup
              ( GroupDto
                  "any"
                  [ ItemCond (CondDto "любимое" "eq" (Just (Bool False)))
                  , ItemCond (CondDto "оценка" "gt" (Just (Number 3)))
                  ]
              )
          ]
    , pdSort = Just (SortFieldsDto [SortItemDto "год" "desc", SortItemDto "название" "asc"])
    , pdLimit = Just 100
    }

jsonTests :: TestTree
jsonTests =
  testGroup
    "JSON-контракт"
    [ testCase "PlaylistDto проходит round-trip через aeson" $
        decode (encode validDto) @?= Just validDto
    ]

------------------------------------------------------------------------------
-- Структурные проверки дерева
------------------------------------------------------------------------------

structuralTests :: TestTree
structuralTests =
  testGroup
    "Структурные ошибки dtoToParsed"
    [ assertDtoError
        "пустое название"
        (simpleDto "   ")
        "/name"
        "не может быть пустым"
    , assertDtoError
        "пустая корневая группа"
        (baseDto (GroupDto "all" []))
        "/root"
        "не может быть пустой"
    , assertDtoError
        "пустая вложенная группа"
        (baseDto (GroupDto "all" [ItemGroup (GroupDto "any" [])]))
        "/root/items/0"
        "не может быть пустой"
    , assertDtoError
        "слишком глубокая вложенность (64)"
        ((simpleDto "Тест") {pdRoot = nestedGroups 64})
        "/root"
        "Слишком глубокая"
    , testCase "глубина 63 допустима" $
        case dtoToParsed ((simpleDto "Тест") {pdRoot = nestedGroups 63}) of
          Right _ -> pure ()
          Left es -> assertFailure ("неожиданные ошибки: " <> show es)
    , assertDtoError
        "условие eq без значения"
        (baseDto (oneCond (CondDto "название" "eq" Nothing)))
        "/root/items/0"
        "не содержит значение"
    , assertDtoError
        "условие between без границ"
        (baseDto (oneCond (CondDto "год" "between" Nothing)))
        "/root/items/0"
        "не содержит значение"
    , assertDtoError
        "raw-узел не редактируется"
        (baseDto (GroupDto "all" [ItemRaw (String "внешний")]))
        "/root/items/0"
        "не может быть отредактирован"
    , assertDtoError
        "неизвестная логика группы"
        (baseDto (GroupDto "xor" [ItemCond plainCond]))
        "/root"
        "Неизвестная логика группы «xor»"
    , assertDtoError
        "пустой список сортировки"
        ((simpleDto "Тест") {pdSort = Just (SortFieldsDto [])})
        "/sort"
        "Список полей сортировки"
    , assertDtoError
        "неизвестное поле сортировки"
        ((simpleDto "Тест") {pdSort = Just (SortFieldsDto [SortItemDto "меме" "asc"])})
        "/sort"
        "Неизвестное поле сортировки «меме»"
    , assertDtoError
        "неизвестное направление сортировки"
        ((simpleDto "Тест") {pdSort = Just (SortFieldsDto [SortItemDto "год" "вверх"])})
        "/sort"
        "Неизвестное направление сортировки"
    , assertDtoError
        "raw-сортировка не поддерживается"
        ((simpleDto "Тест") {pdSort = Just (SortRawDto "значение-из-файла")})
        "/sort"
        "внешнего файла"
    ]

------------------------------------------------------------------------------
-- Конвейер компиляции
------------------------------------------------------------------------------

compileTests :: TestTree
compileTests =
  testGroup
    "compilePlaylistDto"
    [ testCase "валидная подборка: канонический .mix и разбираемый .nsp" $
        case compilePlaylistDto validDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> do
            assertBool
              ("в .mix нет секции «подборка»: " <> T.unpack (cmpMix c))
              ("подборка" `T.isInfixOf` cmpMix c)
            assertBool
              ("в .mix нет корневой группы: " <> T.unpack (cmpMix c))
              ("где все" `T.isInfixOf` cmpMix c)
            case (eitherDecode (cmpNsp c) :: Either String Value) of
              Right (Object o) ->
                KM.lookup "name" o @?= Just (String "Тестовая подборка")
              other -> assertFailure (".nsp не является JSON-объектом: " <> show other)
    , testCase "неизвестное поле: код validation, строка, столбец и путь" $
        case compilePlaylistDto (baseDto (oneCond (CondDto "меме" "gt" (Just (Number 1))))) of
          Right _ -> assertFailure "ожидалась ошибка валидации"
          Left [] -> assertFailure "список ошибок пуст"
          Left (e : _) -> do
            aeCode e @?= "validation"
            assertBool
              ("сообщение не по-русски: " <> T.unpack (aeMessage e))
              ("Неизвестное поле" `T.isInfixOf` aeMessage e)
            assertBool "нет строки" (isJust (aeLine e))
            assertBool "нет столбца" (isJust (aeColumn e))
            assertBool "нет пути до элемента" (maybe False (not . T.null) (aePath e))
    , testCase "противоречивые условия: validation, позиция и путь" $
        let dto =
              (simpleDto "Тест")
                { pdRoot =
                    GroupDto
                      "all"
                      [ ItemCond (CondDto "год" "gt" (Just (Number 2020)))
                      , ItemCond (CondDto "год" "lt" (Just (Number 2000)))
                      ]
                }
         in case compilePlaylistDto dto of
              Right _ -> assertFailure "ожидалась ошибка валидации"
              Left [] -> assertFailure "список ошибок пуст"
              Left (e : _) -> do
                aeCode e @?= "validation"
                assertBool
                  ("нет сообщения о противоречии: " <> T.unpack (aeMessage e))
                  ("противоречит условию" `T.isInfixOf` aeMessage e)
                assertBool "нет строки" (isJust (aeLine e))
                assertBool "нет столбца" (isJust (aeColumn e))
                assertBool "нет пути до элемента" (maybe False (not . T.null) (aePath e))
    , testCase "структурная ошибка: код invalid_tree и путь /name" $
        case compilePlaylistDto (simpleDto "") of
          Right _ -> assertFailure "ожидалась структурная ошибка"
          Left [] -> assertFailure "список ошибок пуст"
          Left (e : _) -> do
            aeCode e @?= "invalid_tree"
            aePath e @?= Just "/name"
            assertBool "нет русского сообщения" (not (T.null (aeMessage e)))
    , testCase "ошибка типов валидации содержит позицию в .mix" $
        case compilePlaylistDto (baseDto (oneCond (CondDto "год" "contains" (Just (String "икра"))))) of
          Right _ -> assertFailure "ожидалась ошибка валидации"
          Left (e : _) -> do
            aeCode e @?= "validation"
            assertBool
              ("нет сообщения о текстовых полях: " <> T.unpack (aeMessage e))
              ("содержит" `T.isInfixOf` aeMessage e)
            assertBool "нет строки" (isJust (aeLine e))
          Left [] -> assertFailure "список ошибок пуст"
    ]

------------------------------------------------------------------------------
-- Обратные переводы
------------------------------------------------------------------------------

roundTripTests :: TestTree
roundTripTests =
  testGroup
    "Обратные переводы"
    [ testCase "dtoFromMix(compile(dto)) == dto" $ case compilePlaylistDto validDto of
        Left es -> assertFailure ("ошибки компиляции: " <> show es)
        Right c -> dtoFromMix (cmpMix c) @?= Right validDto
    , testCase "nspToDto(compile(dto)) == dto" $ case compilePlaylistDto validDto of
        Left es -> assertFailure ("ошибки компиляции: " <> show es)
        Right c -> case (eitherDecode (cmpNsp c) :: Either String Value) of
          Left e -> assertFailure (".nsp не разбирается: " <> e)
          Right v -> nspToDto v @?= Right validDto
    , goldenRoundTrip "forgotten-favorites"
    , goldenRoundTrip "eighties-rock"
    , goldenRoundTrip "missing-metadata"
    , goldenRoundTrip "recent-discoveries"
    , goldenRoundTrip "playlist-links"
    , testCase "dateDto: compile -> dtoFromMix -> тот же DTO" $
        case compilePlaylistDto dateDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> dtoFromMix (cmpMix c) @?= Right dateDto
    , testCase "dateDto: compile -> nspToDto -> тот же DTO" $
        case compilePlaylistDto dateDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> case (eitherDecode (cmpNsp c) :: Either String Value) of
            Left e -> assertFailure (".nsp не разбирается: " <> e)
            Right v -> nspToDto v @?= Right dateDto
    ]

-- | Golden @.mix@ -> DTO -> канонический @.mix@ -> тот же DTO.
goldenRoundTrip :: String -> TestTree
goldenRoundTrip name = testCase name $ do
  src <- goldenSrc name
  dto1 <- case dtoFromMix src of
    Left es -> assertFailure ("dtoFromMix: " <> show es)
    Right d -> pure d
  c <- case compilePlaylistDto dto1 of
    Left es -> assertFailure ("compilePlaylistDto: " <> show es)
    Right r -> pure r
  dtoFromMix (cmpMix c) @?= Right dto1

nspToDtoTests :: TestTree
nspToDtoTests =
  testGroup
    "nspToDto"
    [ testCase "корректный .nsp разбирается в DSL-дерево" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"is\":{\"title\":\"a\"}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                (GroupDto "all" [ItemCond (CondDto "название" "eq" (Just (String "a")))])
                Nothing
                Nothing
            )
    , testCase "чистое дерево не содержит raw" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"is\":{\"title\":\"a\"}}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= False
    , testCase "неизвестное поле -> ItemRaw" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"is\":{\"meme\":1}}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    , testCase "неизвестный оператор -> ItemRaw" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"bogus\":{\"title\":\"a\"}}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    , testCase "raw внутри вложенной группы находится" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"any\":[{\"bogus\":{\"title\":\"a\"}}]}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    , testCase "не-строковый sort -> raw-сортировка" $
        case nspFrom "{\"name\":\"X\",\"all\":[],\"sort\":123}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    , testCase "отсутствует name -> Left" $
        assertBool "ожидался Left" (isLeft (nspFrom "{\"all\":[]}"))
    , testCase "all и any одновременно -> Left" $
        assertBool "ожидался Left" (isLeft (nspFrom "{\"name\":\"X\",\"all\":[],\"any\":[]}"))
    , testCase "корень не массив -> Left" $
        assertBool "ожидался Left" (isLeft (nspFrom "{\"name\":\"X\",\"all\":5}"))
    , testCase "лимит не целое -> Left" $
        assertBool "ожидался Left" (isLeft (nspFrom "{\"name\":\"X\",\"all\":[],\"limit\":\"x\"}"))
    , testCase "before с датой -> условие DSL" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"before\":{\"lastplayed\":\"2024-06-01\"}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                ( GroupDto
                    "all"
                    [ ItemCond (CondDto "последнее_прослушивание" "before" (Just (String "2024-06-01"))) ]
                )
                Nothing
                Nothing
            )
    , testCase "after с датой -> условие DSL" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"after\":{\"dateadded\":\"2024-01-01\"}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                (GroupDto "all" [ItemCond (CondDto "добавлено" "after" (Just (String "2024-01-01")))])
                Nothing
                Nothing
            )
    , testCase "inTheRange с датами -> между" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"inTheRange\":{\"dateadded\":[\"2024-01-01\",\"2024-12-31\"]}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                ( GroupDto
                    "all"
                    [ ItemCond
                        ( CondDto
                            "добавлено"
                            "between"
                            (Just (toJSON [String "2024-01-01", String "2024-12-31"]))
                        )
                    ]
                )
                Nothing
                Nothing
            )
    , testCase "before без корректной даты -> raw" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"before\":{\"lastplayed\":\"вчера\"}}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    , testCase "inTheRange с не-датовыми строками -> raw" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"inTheRange\":{\"dateadded\":[\"a\",\"b\"]}}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    , testCase "файл не объект -> Left" $
        assertBool "ожидался Left" (isLeft (nspFrom "[1,2]"))
    ]

------------------------------------------------------------------------------
-- Дублирующиеся поля
------------------------------------------------------------------------------

-- | «Любимое = да AND Год > 2010 AND Год < 2020»: поле год
-- повторяется — оба условия обязаны сохраниться независимо
-- и в исходном порядке на всём пути DTO → .mix → .nsp.
dupDto :: PlaylistDto
dupDto =
  (simpleDto "Дубли полей")
    { pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "любимое" "eq" (Just (Bool True)))
          , ItemCond (CondDto "год" "gt" (Just (Number 2010)))
          , ItemCond (CondDto "год" "lt" (Just (Number 2020)))
          ]
    }

-- | Тот же DTO после раунд-трипа через .mix: DSL-сахар
-- «любимое» ≡ «любимое = да» нормализуется в bare
-- (VBool True -> bare в 'condItemDto'); оба ограничения года
-- должны пройти без изменений.
dupDtoFromMix :: PlaylistDto
dupDtoFromMix =
  dupDto
    { pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "любимое" "bare" Nothing)
          , ItemCond (CondDto "год" "gt" (Just (Number 2010)))
          , ItemCond (CondDto "год" "lt" (Just (Number 2020)))
          ]
    }

-- | Индекс первого вхождения подстроки; @maxBound@ — не найдено.
indexOf :: Text -> Text -> Int
indexOf needle hay =
  let (before, rest) = T.breakOn needle hay
   in if T.null rest then maxBound else T.length before

-- | JSON, который редактор отправляет на сервер: у каждого элемента
-- есть служебное поле id — оно не влияет на разбор и порядок.
withClientIds :: LBS.ByteString
withClientIds =
  encode $
    object
      [ "name" .= ("Дубли полей" :: Text)
      , "public" .= False
      , "root"
          .= object
            [ "kind" .= ("all" :: Text)
            , "items"
                .= [ object
                       [ "type" .= ("cond" :: Text)
                       , "id" .= ("c-1" :: Text)
                       , "field" .= ("любимое" :: Text)
                       , "op" .= ("eq" :: Text)
                       , "value" .= True
                       ]
                   , object
                       [ "type" .= ("cond" :: Text)
                       , "id" .= ("c-2" :: Text)
                       , "field" .= ("год" :: Text)
                       , "op" .= ("gt" :: Text)
                       , "value" .= (2010 :: Integer)
                       ]
                   , object
                       [ "type" .= ("cond" :: Text)
                       , "id" .= ("c-3" :: Text)
                       , "field" .= ("год" :: Text)
                       , "op" .= ("lt" :: Text)
                       , "value" .= (2020 :: Integer)
                       ]
                   ]
            ]
      ]

duplicateFieldTests :: TestTree
duplicateFieldTests =
  testGroup
    "Дублирующиеся поля"
    [ testCase ".mix: оба условия год на месте и в порядке DTO" $
        case compilePlaylistDto dupDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> do
            let mix = cmpMix c
            assertBool ("нет любимое = да: " <> T.unpack mix) ("любимое = да" `T.isInfixOf` mix)
            assertBool ("нет год > 2010: " <> T.unpack mix) ("год > 2010" `T.isInfixOf` mix)
            assertBool ("нет год < 2020: " <> T.unpack mix) ("год < 2020" `T.isInfixOf` mix)
            assertBool
              "порядок условий в .mix нарушен"
              ( indexOf "любимое" mix
                  < indexOf "год > 2010" mix
                  && indexOf "год > 2010" mix < indexOf "год < 2020" mix
              )
    , testCase ".nsp: оба ограничения год на месте и в порядке DTO" $
        case compilePlaylistDto dupDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> do
            let nsp = TE.decodeUtf8 (LBS.toStrict (cmpNsp c))
            assertBool ("нет 2010: " <> T.unpack nsp) ("2010" `T.isInfixOf` nsp)
            assertBool ("нет 2020: " <> T.unpack nsp) ("2020" `T.isInfixOf` nsp)
            assertBool "порядок год в .nsp нарушен" (indexOf "2010" nsp < indexOf "2020" nsp)
            case (eitherDecode (cmpNsp c) :: Either String Value) of
              Right (Object o) ->
                case KM.lookup "all" o of
                  Just (Array _) -> pure ()
                  other -> assertFailure (".nsp без корневого массива all: " <> show other)
              other -> assertFailure (".nsp не является JSON-объектом: " <> show other)
    , testCase "dtoFromMix(compile(dup)) == dup с нормализацией сахара" $
        case compilePlaylistDto dupDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> dtoFromMix (cmpMix c) @?= Right dupDtoFromMix
    , testCase "nspToDto(compile(dup)) == dup" $
        case compilePlaylistDto dupDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> case (eitherDecode (cmpNsp c) :: Either String Value) of
            Left e -> assertFailure (".nsp не разбирается: " <> e)
            Right v -> nspToDto v @?= Right dupDto
    , testCase "JSON редактора с client-id разбирается в тот же DTO" $
        decode withClientIds @?= Just dupDto
    ]

------------------------------------------------------------------------------
-- Дробные числа (ReplayGain)
------------------------------------------------------------------------------

-- | Подборка с дробными значениями ReplayGain: сравнение и границы.
decimalDto :: PlaylistDto
decimalDto =
  (simpleDto "Дробный ReplayGain")
    { pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "replaygain" "gt" (Just (Number (-6.5))))
          , ItemCond (CondDto "replaygain" "between" (Just (toJSON [Number (-8), Number (-4.5)])))
          ]
    }

decimalTests :: TestTree
decimalTests =
  testGroup
    "Дробные значения ReplayGain"
    [ testCase ".mix/.nsp сохраняют дробные значения" $
        case compilePlaylistDto decimalDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> do
            let mix = cmpMix c
                nsp = TE.decodeUtf8 (LBS.toStrict (cmpNsp c))
            assertBool ("нет -6.5 в .mix: " <> T.unpack mix) ("-6.5" `T.isInfixOf` mix)
            assertBool ("нет -4.5 в .mix: " <> T.unpack mix) ("-4.5" `T.isInfixOf` mix)
            assertBool ("нет -6.5 в .nsp: " <> T.unpack nsp) ("-6.5" `T.isInfixOf` nsp)
            assertBool ("нет -4.5 в .nsp: " <> T.unpack nsp) ("-4.5" `T.isInfixOf` nsp)
    , testCase "dtoFromMix(compile(decimal)) == decimal" $
        case compilePlaylistDto decimalDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> dtoFromMix (cmpMix c) @?= Right decimalDto
    , testCase "nspToDto(compile(decimal)) == decimal" $
        case compilePlaylistDto decimalDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> case (eitherDecode (cmpNsp c) :: Either String Value) of
            Left e -> assertFailure (".nsp не разбирается: " <> e)
            Right v -> nspToDto v @?= Right decimalDto
    , testCase "nspToDto: дробный .nsp разбирается в DSL-дерево" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"gt\":{\"rgtrackgain\":-6.5}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                (GroupDto "all" [ItemCond (CondDto "replaygain" "gt" (Just (Number (-6.5))))])
                Nothing
                Nothing
            )
    ]

------------------------------------------------------------------------------
-- Датовые условия
------------------------------------------------------------------------------

-- | Подборка с абсолютными датами: сравнение, «до», диапазон дат.
dateDto :: PlaylistDto
dateDto =
  (simpleDto "Свежие находки")
    { pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "последнее_прослушивание" "before" (Just (String "2024-06-01")))
          , ItemCond (CondDto "добавлено" "between" (Just (toJSON [String "2024-01-01", String "2024-12-31"])))
          , ItemCond (CondDto "последнее_прослушивание" "ne" (Just (String "2020-01-01")))
          ]
    }

------------------------------------------------------------------------------
-- Членство в подборке (inPlaylist / notInPlaylist)
------------------------------------------------------------------------------

-- | Операнд членства в DTO: @{kind, value}@.
linkValue :: Text -> Text -> Value
linkValue kind value = object ["kind" .= kind, "value" .= value]

-- | Подборка с двумя условиями членства: по id и по пути к файлу.
playlistDto :: PlaylistDto
playlistDto =
  (simpleDto "Ссылки")
    { pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "подборка" "inPlaylist" (Just (linkValue "id" "abc-123")))
          , ItemCond (CondDto "подборка" "notInPlaylist" (Just (linkValue "path" "other.nsp")))
          ]
    }

playlistLinkTests :: TestTree
playlistLinkTests =
  testGroup
    "Членство в подборке"
    [ testCase ".mix/.nsp сохраняют членство в подборке" $
        case compilePlaylistDto playlistDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> do
            let mix = cmpMix c
                nsp = TE.decodeUtf8 (LBS.toStrict (cmpNsp c))
            assertBool
              ("нет «в подборке id» в .mix: " <> T.unpack mix)
              ("в подборке id \"abc-123\"" `T.isInfixOf` mix)
            assertBool
              ("нет «не в подборке файл» в .mix: " <> T.unpack mix)
              ("не в подборке файл \"other.nsp\"" `T.isInfixOf` mix)
            assertBool ("нет inPlaylist в .nsp: " <> T.unpack nsp) ("inPlaylist" `T.isInfixOf` nsp)
            assertBool
              ("нет notInPlaylist в .nsp: " <> T.unpack nsp)
              ("notInPlaylist" `T.isInfixOf` nsp)
    , testCase "dtoFromMix(compile(playlist)) == playlist" $
        case compilePlaylistDto playlistDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> dtoFromMix (cmpMix c) @?= Right playlistDto
    , testCase "nspToDto(compile(playlist)) == playlist" $
        case compilePlaylistDto playlistDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> case (eitherDecode (cmpNsp c) :: Either String Value) of
            Left e -> assertFailure (".nsp не разбирается: " <> e)
            Right v -> nspToDto v @?= Right playlistDto
    , testCase "пустая ссылка: ошибка валидации" $
        let dto =
              (simpleDto "Ссылки")
                { pdRoot =
                    GroupDto "all" [ItemCond (CondDto "подборка" "inPlaylist" (Just (linkValue "id" "")))]
                }
         in case compilePlaylistDto dto of
              Right _ -> assertFailure "ожидалась ошибка валидации"
              Left [] -> assertFailure "список ошибок пуст"
              Left es ->
                assertBool
                  ("нет сообщения о пустом идентификаторе: " <> show (map aeMessage es))
                  (any ("Идентификатор подборки не может быть пустым" `T.isInfixOf`) (map aeMessage es))
    , assertDtoError
        "оператор только для поля подборка"
        (baseDto (oneCond (CondDto "название" "inPlaylist" (Just (linkValue "id" "abc")))))
        "/root/items/0"
        "применим только к полю"
    , testCase "nspToDto: inPlaylist с id -> условие DSL (не raw)" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"inPlaylist\":{\"id\":\"abc-123\"}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                (GroupDto "all" [ItemCond (CondDto "подборка" "inPlaylist" (Just (linkValue "id" "abc-123")))])
                Nothing
                Nothing
            )
    , testCase "nspToDto: notInPlaylist с path -> условие DSL (не raw)" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"notInPlaylist\":{\"path\":\"other.nsp\"}}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> do
            containsRawDto dto @?= False
            dto
              @?= PlaylistDto
                "X"
                Nothing
                False
                ( GroupDto
                    "all"
                    [ ItemCond (CondDto "подборка" "notInPlaylist" (Just (linkValue "path" "other.nsp"))) ]
                )
                Nothing
                Nothing
    , testCase "nspToDto: неизвестный вид ссылки -> raw" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"inPlaylist\":{\"title\":\"a\"}}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    , testCase "nspToDto: не-объект ссылки -> raw" $
        case nspFrom "{\"name\":\"X\",\"all\":[{\"inPlaylist\":\"abc\"}]}" of
          Left e -> assertFailure (T.unpack e)
          Right dto -> containsRawDto dto @?= True
    ]

------------------------------------------------------------------------------
-- Поля реестра (e2e: DTO → NSP)
------------------------------------------------------------------------------

-- | Единая заготовка: подборка с одним условием по новому полю.
newCond :: Text -> Text -> Maybe Value -> PlaylistDto
newCond fld op mv = baseDto (oneCond (CondDto fld op mv))

registryFieldDtoTests :: TestTree
registryFieldDtoTests =
  testGroup
    "Поля реестра (e2e DTO)"
    [ assertNspContains
        "целое поле: номер_трека"
        (newCond "номер_трека" "eq" (Just (Number 3)))
        ["\"tracknumber\": 3"]
    , assertNspContains
        "дробное поле: длительность"
        (newCond "длительность" "gt" (Just (Number 200.5)))
        ["\"duration\": 200.5"]
    , assertNspContains
        "текстовое поле: кодек"
        (newCond "кодек" "eq" (Just (String "MP3")))
        ["\"codec\": \"MP3\""]
    , assertNspContains
        "наличие: темп отсутствует"
        (newCond "темп" "isMissing" Nothing)
        ["\"isMissing\"", "\"bpm\""]
    , assertNspContains
        "логическое поле: сборник"
        (newCond "сборник" "eq" (Just (Bool True)))
        ["\"compilation\": true"]
    , assertNspContains
        "дата: дата_релиза"
        (newCond "дата_релиза" "after" (Just (String "2020-01-01")))
        ["\"after\"", "\"releasedate\": \"2020-01-01\""]
    , assertNspContains
        "альбомное поле: прослушиваний_альбома"
        (newCond "прослушиваний_альбома" "gt" (Just (Number 10)))
        ["\"albumplaycount\": 10"]
    , assertNspContains
        "артистное поле: оценка_артиста"
        (newCond "оценка_артиста" "eq" (Just (Number 5)))
        ["\"artistrating\": 5"]
    , assertNspContains
        "MusicBrainz ID: mbid_артиста"
        (newCond "mbid_артиста" "eq" (Just (String "abc-123")))
        ["\"mbz_artist_id\": \"abc-123\""]
    , assertNspContains
        "алиас replaygain_track_gain => rgtrackgain"
        (newCond "replaygain_track_gain" "gt" (Just (Number (-6.5))))
        ["\"rgtrackgain\": -6.5"]
    , assertNspContains
        "алиас replaygain_track_peak => rgtrackpeak"
        (newCond "replaygain_track_peak" "isMissing" Nothing)
        ["\"rgtrackpeak\""]
    , assertNspContains
        "алиас replaygain_album_gain => rgalbumgain"
        (newCond "replaygain_album_gain" "gt" (Just (Number (-1.5))))
        ["\"rgalbumgain\": -1.5"]
    , assertNspContains
        "алиас replaygain_album_peak => rgalbumpeak"
        (newCond "replaygain_album_peak" "gt" (Just (Number (-0.997))))
        ["\"rgalbumpeak\": -0.997"]
    , assertNspContains
        "алиас lastPlayed в сортировке => -lastplayed"
        ((newCond "любимое" "eq" (Just (Bool True))) {pdSort = Just (SortFieldsDto [SortItemDto "lastPlayed" "desc"])})
        ["\"sort\": \"-lastplayed\""]
    , assertCompileError
        "дробь на целочисленном поле"
        (newCond "номер_трека" "eq" (Just (Number 2.5)))
        "должно быть целым числом"
    , assertCompileError
        "наличие на поле без признака"
        (newCond "библиотека" "isMissing" Nothing)
        "не поддерживает проверку наличия"
    , assertCompileError
        "значение вне границ рейтинга альбома"
        (newCond "оценка_альбома" "eq" (Just (Number 7)))
        "вне допустимого диапазона"
    ]

------------------------------------------------------------------------------
-- Регистр имён без учёта регистра (как Navidrome LookupField)
------------------------------------------------------------------------------

-- | Обратный перевод и компиляция с другим регистром имён: поле и
-- сортировка канонизируются в DSL-/NSP-имена реестра, исходное
-- написание не протекает ни в @.nsp@, ни в @.mix@-раунд-трип.
caseInsensitiveDtoTests :: TestTree
caseInsensitiveDtoTests =
  testGroup
    "Регистр имён"
    [ testCase "nspToDto: NSP-имя в верхнем регистре -> DSL-имя" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"is\":{\"TITLE\":\"a\"}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                (GroupDto "all" [ItemCond (CondDto "название" "eq" (Just (String "a")))])
                Nothing
                Nothing
            )
    , testCase "nspToDto: алиас в верхнем регистре -> своя запись" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"gt\":{\"REPLAYGAIN_ALBUM_GAIN\":-1.5}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                ( GroupDto
                    "all"
                    [ ItemCond (CondDto "replaygain_альбом" "gt" (Just (Number (-1.5)))) ]
                )
                Nothing
                Nothing
            )
    , testCase "nspToDto: алиас replaygain_album_peak -> своя запись" $
        nspFrom "{\"name\":\"X\",\"all\":[{\"gt\":{\"replaygain_album_peak\":-0.997}}]}"
          @?= Right
            ( PlaylistDto
                "X"
                Nothing
                False
                ( GroupDto
                    "all"
                    [ ItemCond (CondDto "replaygain_пик_альбом" "gt" (Just (Number (-0.997)))) ]
                )
                Nothing
                Nothing
            )
    , testCase "nspToDto: sort в разном регистре -> DSL-имя" $
        case nspFrom "{\"name\":\"X\",\"all\":[],\"sort\":\"-PlayCount\"}" of
          Left e -> assertFailure (T.unpack e)
          Right dto ->
            pdSort dto @?= Just (SortFieldsDto [SortItemDto "прослушиваний" "desc"])
    , testCase "compilePlaylistDto: смешанный регистр -> канонический .nsp" $
        case compilePlaylistDto mixedDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> do
            let nsp = TE.decodeUtf8 (LBS.toStrict (cmpNsp c))
            assertBool
              ("нет \"title\": \"Тест\" в .nsp: " <> T.unpack nsp)
              ("\"title\": \"Тест\"" `T.isInfixOf` nsp)
            assertBool
              ("нет \"playcount\": 5 в .nsp: " <> T.unpack nsp)
              ("\"playcount\": 5" `T.isInfixOf` nsp)
            assertBool
              ("сортировка не каноническая: " <> T.unpack nsp)
              ("\"sort\": \"title\"" `T.isInfixOf` nsp)
            assertBool
              ("протекло TITLE: " <> T.unpack nsp)
              (not ("TITLE" `T.isInfixOf` nsp))
            assertBool
              ("протекло ПРОСЛУШИВАНИЙ: " <> T.unpack nsp)
              (not ("ПРОСЛУШИВАНИЙ" `T.isInfixOf` nsp))
    , testCase "dtoFromMix(compile(смешанный регистр)) == канонический DTO" $
        case compilePlaylistDto mixedDto of
          Left es -> assertFailure ("ошибки компиляции: " <> show es)
          Right c -> dtoFromMix (cmpMix c) @?= Right mixedDtoCanonical
    ]

-- | Подборка с полями и сортировкой в верхнем регистре.
mixedDto :: PlaylistDto
mixedDto =
  (simpleDto "Регистр")
    { pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "TITLE" "eq" (Just (String "Тест")))
          , ItemCond (CondDto "ПРОСЛУШИВАНИЙ" "gt" (Just (Number 5)))
          ]
    , pdSort = Just (SortFieldsDto [SortItemDto "TITLE" "asc"])
    }

-- | Тот же DTO после раунд-трипа: все имена канонические.
mixedDtoCanonical :: PlaylistDto
mixedDtoCanonical =
  mixedDto
    { pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "название" "eq" (Just (String "Тест")))
          , ItemCond (CondDto "прослушиваний" "gt" (Just (Number 5)))
          ]
    , pdSort = Just (SortFieldsDto [SortItemDto "название" "asc"])
    }

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

dtoTests :: TestTree
dtoTests =
  testGroup
    "DTO"
    [ jsonTests
    , structuralTests
    , compileTests
    , roundTripTests
    , nspToDtoTests
    , duplicateFieldTests
    , decimalTests
    , playlistLinkTests
    , registryFieldDtoTests
    , caseInsensitiveDtoTests
    ]
