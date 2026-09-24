{-# LANGUAGE OverloadedStrings #-}

-- | Unit-тесты: каждое поле, каждый оператор, вложенные группы,
-- синтаксический сахар, ошибки типов, сортировка, необязательные
-- метаданные и точный формат сообщений об ошибках.
module UnitTests (unitTests) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Nspeller.Ast
import Nspeller.Compiler (compileText)
import Test.Tasty
import Test.Tasty.HUnit

------------------------------------------------------------------------------
-- Вспомогательные функции
------------------------------------------------------------------------------

-- | Аннотации для литералов в ожидаемых JSON-значениях.
t :: Text -> Text
t = id

i :: Integer -> Integer
i = id

-- | Подборка с одним условием внутри @где все@.
wrap :: Text -> Text
wrap cond = wrapGroup "все" [cond]

-- | Подборка с условиями внутри группы заданного вида («все» или
-- «любое»).
wrapGroup :: Text -> [Text] -> Text
wrapGroup kind conds =
  T.unlines $
    ["подборка \"Тест\"", "где " <> kind <> " {"]
      ++ map ("  " <>) conds
      ++ ["}"]

renderAll :: [CompileError] -> Text
renderAll = T.intercalate "\n\n" . map renderCompileError

-- | Источник должен скомпилироваться ровно в данное JSON-значение.
assertCompilesTo :: TestName -> Text -> Value -> TestTree
assertCompilesTo name src expected = testCase name $
  case compileText "test.mix" src of
    Left errs ->
      assertFailure ("ошибка компиляции:\n" <> T.unpack (renderAll errs))
    Right nsp -> toJSON nsp @?= expected

-- | Источник должен завершиться ошибкой с сообщением, содержащим
-- данный фрагмент.
assertErrorContains :: TestName -> Text -> Text -> TestTree
assertErrorContains name src needle = testCase name $
  case compileText "test.mix" src of
    Right _ -> assertFailure "ожидалась ошибка компиляции, но файл валиден"
    Left errs ->
      let combined = T.intercalate "\n" (map messagesOf errs)
          messagesOf e = T.intercalate "\n" (NE.toList (ceMessages e))
       in assertBool
            ( "сообщение не содержит «"
                <> T.unpack needle
                <> "»; получено:\n"
                <> T.unpack combined
            )
            (needle `T.isInfixOf` combined)

-- | Ошибка компиляции должна указывать на данную позицию.
assertErrorPos :: TestName -> Text -> (Int, Int) -> TestTree
assertErrorPos name src expectedPos = testCase name $
  case compileText "test.mix" src of
    Right _ -> assertFailure "ожидалась ошибка компиляции, но файл валиден"
    Left (e : _) -> cePos e @?= Just expectedPos
    Left [] -> assertFailure "список ошибок пуст"

-- | Источник должен скомпилироваться без ошибок.
assertValid :: TestName -> Text -> TestTree
assertValid name src = testCase name $
  case compileText "test.mix" src of
    Left errs -> assertFailure ("ошибка компиляции:\n" <> T.unpack (renderAll errs))
    Right _ -> pure ()

-- | Источник должен завершиться ошибкой ровно с данным числом
-- диагностик.
assertErrorCount :: TestName -> Text -> Int -> TestTree
assertErrorCount name src expected = testCase name $
  case compileText "test.mix" src of
    Right _ -> assertFailure "ожидалась ошибка компиляции, но файл валиден"
    Left errs ->
      assertEqual
        ("получено:\n" <> T.unpack (renderAll errs))
        expected
        (length errs)

-- | JSON не должен содержать перечисленных ключей.
assertLacksKeys :: TestName -> Text -> [Key] -> TestTree
assertLacksKeys name src keys = testCase name $
  case compileText "test.mix" src of
    Left errs -> assertFailure ("ошибка компиляции:\n" <> T.unpack (renderAll errs))
    Right nsp -> case toJSON nsp of
      Object o ->
        assertBool
          ("в JSON есть лишние ключи: " <> show keys)
          (all (\k -> KM.lookup k o == Nothing) keys)
      _ -> assertFailure "ожидался JSON-объект"

------------------------------------------------------------------------------
-- Поля
------------------------------------------------------------------------------

fieldTests :: TestTree
fieldTests =
  testGroup
    "Поля"
    [ assertCompilesTo
        "название"
        (wrap "название = \"Hello\"")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["title" .= t "Hello"]]]])
    , assertCompilesTo
        "альбом"
        (wrap "альбом != \"Best\"")
        (object ["name" .= t "Тест", "all" .= [object ["isNot" .= object ["album" .= t "Best"]]]])
    , assertCompilesTo
        "жанр"
        (wrap "жанр содержит \"rock\"")
        (object ["name" .= t "Тест", "all" .= [object ["contains" .= object ["genre" .= t "rock"]]]])
    , assertCompilesTo
        "год"
        (wrap "год > 1990")
        (object ["name" .= t "Тест", "all" .= [object ["gt" .= object ["year" .= i 1990]]]])
    , assertCompilesTo
        "оценка"
        (wrap "оценка < 5")
        (object ["name" .= t "Тест", "all" .= [object ["lt" .= object ["rating" .= i 5]]]])
    , assertCompilesTo
        "прослушиваний"
        (wrap "прослушиваний = 10")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["playcount" .= i 10]]]])
    , assertCompilesTo
        "любимое (сахар)"
        (wrap "любимое")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["loved" .= True]]]])
    , assertCompilesTo
        "последнее_прослушивание"
        (wrap "последнее_прослушивание за 7 дней")
        (object ["name" .= t "Тест", "all" .= [object ["inTheLast" .= object ["lastplayed" .= i 7]]]])
    , assertCompilesTo
        "добавлено"
        (wrap "добавлено за 30 дней")
        (object ["name" .= t "Тест", "all" .= [object ["inTheLast" .= object ["dateadded" .= i 30]]]])
    , assertCompilesTo
        "explicit"
        (wrap "explicit = \"explicit\"")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["explicitstatus" .= t "explicit"]]]])
    , assertCompilesTo
        "обложка"
        (wrap "обложка = нет")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["hascoverart" .= False]]]])
    , assertCompilesTo
        "replaygain"
        (wrap "replaygain присутствует")
        (object ["name" .= t "Тест", "all" .= [object ["isPresent" .= object ["rgtrackgain" .= True]]]])
    , assertCompilesTo
        "replaygain: отсутствует"
        (wrap "replaygain отсутствует")
        (object ["name" .= t "Тест", "all" .= [object ["isMissing" .= object ["rgtrackgain" .= True]]]])
    ]

------------------------------------------------------------------------------
-- Операторы
------------------------------------------------------------------------------

operatorTests :: TestTree
operatorTests =
  testGroup
    "Операторы"
    [ assertCompilesTo
        "="
        (wrap "год = 2000")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["year" .= i 2000]]]])
    , assertCompilesTo
        "!="
        (wrap "оценка != 2")
        (object ["name" .= t "Тест", "all" .= [object ["isNot" .= object ["rating" .= i 2]]]])
    , assertCompilesTo
        ">"
        (wrap "прослушиваний > 100")
        (object ["name" .= t "Тест", "all" .= [object ["gt" .= object ["playcount" .= i 100]]]])
    , assertCompilesTo
        "<"
        (wrap "год < 1990")
        (object ["name" .= t "Тест", "all" .= [object ["lt" .= object ["year" .= i 1990]]]])
    , assertCompilesTo
        "между"
        (wrap "год между 1980 и 1989")
        ( object
            ["name" .= t "Тест", "all" .= [object ["inTheRange" .= object ["year" .= [i 1980, i 1989]]]]]
        )
    , assertCompilesTo
        "содержит"
        (wrap "жанр содержит \"rock\"")
        (object ["name" .= t "Тест", "all" .= [object ["contains" .= object ["genre" .= t "rock"]]]])
    , assertCompilesTo
        "не содержит"
        (wrap "жанр не содержит \"pop\"")
        (object ["name" .= t "Тест", "all" .= [object ["notContains" .= object ["genre" .= t "pop"]]]])
    , assertCompilesTo
        "начинается с"
        (wrap "жанр начинается с \"alt\"")
        (object ["name" .= t "Тест", "all" .= [object ["startsWith" .= object ["genre" .= t "alt"]]]])
    , assertCompilesTo
        "заканчивается на"
        (wrap "жанр заканчивается на \"metal\"")
        (object ["name" .= t "Тест", "all" .= [object ["endsWith" .= object ["genre" .= t "metal"]]]])
    , assertCompilesTo
        "обложка = да"
        (wrap "обложка = да")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["hascoverart" .= True]]]])
    , assertCompilesTo
        "любимое = нет"
        (wrap "любимое = нет")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["loved" .= False]]]])
    , assertCompilesTo
        "за N дней (inTheLast)"
        (wrap "добавлено за 45 дней")
        (object ["name" .= t "Тест", "all" .= [object ["inTheLast" .= object ["dateadded" .= i 45]]]])
    , assertCompilesTo
        "не звучало N дней (notInTheLast)"
        (wrap "не звучало 90 дней")
        (object ["name" .= t "Тест", "all" .= [object ["notInTheLast" .= object ["lastplayed" .= i 90]]]])
    ]

------------------------------------------------------------------------------
-- Группы, сортировка, метаданные
------------------------------------------------------------------------------

groupTests :: TestTree
groupTests =
  testGroup
    "Группы и сахар"
    [ assertCompilesTo
        "вложенные все/любое"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  год между 1980 и 1989"
            , "  любое {"
            , "    любимое"
            , "    оценка > 3"
            , "  }"
            , "}"
            ]
        )
        ( object
            [ "name" .= t "Тест"
            ,
              "all"
                .= [ object ["inTheRange" .= object ["year" .= [i 1980, i 1989]]]
                   , object ["any" .= [object ["is" .= object ["loved" .= True]], object ["gt" .= object ["rating" .= i 3]]]]
                   ]
            ]
        )
    , assertCompilesTo
        "корень где любое"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где любое {"
            , "  жанр = \"jazz\""
            , "  жанр = \"blues\""
            , "}"
            ]
        )
        ( object
            [ "name" .= t "Тест"
            ,
              "any"
                .= [ object ["is" .= object ["genre" .= t "jazz"]]
                   , object ["is" .= object ["genre" .= t "blues"]]
                   ]
            ]
        )
    , assertCompilesTo
        "комментарии игнорируются"
        ( T.unlines
            [ "# комментарий в начале файла"
            , "подборка \"Тест\" # хвостовой комментарий"
            , "где все {"
            , "  любимое # условие с комментарием"
            , "}"
            ]
        )
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["loved" .= True]]]])
    , assertCompilesTo
        "CRLF-окончания строк"
        "подборка \"Тест\"\r\nгде все {\r\n  любимое\r\n}\r\n"
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["loved" .= True]]]])
    ]

metadataTests :: TestTree
metadataTests =
  testGroup
    "Метаданные и сортировка"
    [ assertCompilesTo
        "полные метаданные + публичная"
        ( T.unlines
            [ "подборка \"Избранное\""
            , "описание \"Любимые треки\""
            , "публичная"
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок случайный"
            , "лимит 100"
            ]
        )
        ( object
            [ "name" .= t "Избранное"
            , "comment" .= t "Любимые треки"
            , "public" .= True
            , "all" .= [object ["is" .= object ["loved" .= True]]]
            , "sort" .= t "random"
            , "limit" .= i 100
            ]
        )
    , assertLacksKeys
        "необязательные секции опущены"
        ( T.unlines
            [ "подборка \"Минимум\""
            , "где все {"
            , "  любимое"
            , "}"
            ]
        )
        ["comment", "public", "sort", "limit"]
    , assertCompilesTo
        "порядок: несколько полей"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок {"
            , "  год убыв"
            , "  оценка убыв"
            , "  название возр"
            , "}"
            ]
        )
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["is" .= object ["loved" .= True]]]
            , "sort" .= t "-year,-rating,title"
            ]
        )
    , assertCompilesTo
        "порядок: случайный"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок случайный"
            ]
        )
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["is" .= object ["loved" .= True]]]
            , "sort" .= t "random"
            ]
        )
    , assertCompilesTo
        "порядок: датовое поле"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок {"
            , "  добавлено убыв"
            , "}"
            ]
        )
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["is" .= object ["loved" .= True]]]
            , "sort" .= t "-dateadded"
            ]
        )
    ]

------------------------------------------------------------------------------
-- Ошибки
------------------------------------------------------------------------------

errorTests :: TestTree
errorTests =
  testGroup
    "Ошибки"
    [ testCase "точный формат ошибки (спецификация)" $
        let src =
              T.unlines
                [ "подборка \"Тест\""
                , "описание \"описание\""
                , "публичная"
                , "где все {"
                , "  оценка содержит \"rock\""
                , "}"
                ]
            expected =
              T.unlines
                [ "broken.mix:5:3"
                , ""
                , "  оценка содержит \"rock\""
                , "  ^^^^^^^^^^^^^^^^^^^^^^"
                , ""
                , "Оператор «содержит» применим только к текстовым полям."
                , "Поле «оценка» имеет числовой тип."
                ]
         in case compileText "broken.mix" src of
              Right _ -> assertFailure "ожидалась ошибка компиляции"
              Left (e : _) -> (renderCompileError e <> "\n") @?= expected
              Left [] -> assertFailure "список ошибок пуст"
    , assertErrorContains "неизвестное поле" (wrap "меме > 1") "Неизвестное поле «меме»."
    , assertErrorContains
        "любимое > 3"
        (wrap "любимое > 3")
        "Оператор «>» применим только к числовым полям."
    , assertErrorContains
        "любимое > 3: тип поля"
        (wrap "любимое > 3")
        "Поле «любимое» имеет логический тип."
    , assertErrorContains
        "жанр > 10"
        (wrap "жанр > 10")
        "Поле «жанр» имеет текстовый тип."
    , assertErrorContains
        "оценка содержит"
        (wrap "оценка содержит \"rock\"")
        "Оператор «содержит» применим только к текстовым полям."
    , assertErrorContains
        "добавлено содержит"
        (wrap "добавлено содержит \"вчера\"")
        "Поле «добавлено» имеет тип даты."
    , assertErrorContains
        "значение не того типа"
        (wrap "год = \"abc\"")
        "ожидается число."
    , assertErrorContains
        "неположительный лимит"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "лимит 0"
            ]
        )
        "положительным целым числом, получено 0"
    , assertErrorContains
        "отрицательный лимит"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "лимит -5"
            ]
        )
        "положительным целым числом, получено -5"
    , assertErrorContains
        "нет секции подборка"
        (T.unlines ["где все {", "  любимое", "}"])
        "Отсутствует обязательная секция «подборка»."
    , assertErrorContains
        "нет секции где"
        (T.unlines ["подборка \"Тест\""])
        "Отсутствует обязательная секция «где»."
    , assertErrorContains
        "дубликат секции подборка"
        (T.unlines ["подборка \"Первая\"", "подборка \"Вторая\"", "где все {", "  любимое", "}"])
        "Секция «подборка» указана повторно."
    , assertErrorPos
        "дубликат указывает на второе вхождение"
        (T.unlines ["подборка \"Первая\"", "подборка \"Вторая\"", "где все {", "  любимое", "}"])
        (2, 1)
    , assertErrorContains
        "булево поле в сортировке"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок {"
            , "  любимое возр"
            , "}"
            ]
        )
        "нельзя использовать для сортировки"
    , assertErrorContains
        "неизвестное поле сортировки"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок {"
            , "  меме возр"
            , "}"
            ]
        )
        "Неизвестное поле сортировки «меме»."
    , assertErrorContains
        "сокращённая запись не для булева поля"
        (wrap "жанр")
        "Сокращённая запись допустима только для логических полей."
    , assertErrorContains
        "проверка наличия не для всех полей"
        (wrap "любимое присутствует")
        "не поддерживает проверку наличия"
    , assertErrorContains
        "сравнение даты не поддерживается"
        (wrap "добавлено = \"2020-01-01\"")
        "Для датовых полей поддерживается только сравнение"
    , assertErrorContains
        "неположительное число дней"
        (wrap "добавлено за 0 дней")
        "Число дней должно быть положительным, получено 0"
    , assertErrorContains
        "перевёрнутый диапазон"
        (wrap "год между 1990 и 1980")
        "Нижняя граница диапазона не может быть больше верхней"
    , assertErrorContains
        "за N дней не для датового поля"
        (wrap "жанр за 5 дней")
        "Сравнение «за … дней» применимо только к датовым полям"
    , testCase "собираются все ошибки файла, а не только первая" $
        let src =
              T.unlines
                [ "подборка \"Тест\""
                , "где все {"
                , "  меме > 1"
                , "  оценка содержит \"rock\""
                , "  жанр за 5 дней"
                , "}"
                ]
         in case compileText "test.mix" src of
              Right _ -> assertFailure "ожидалась ошибка компиляции"
              Left errs -> do
                length errs @?= 3
                let combined = T.intercalate "\n" (map renderCompileError errs)
                assertBool "нет ошибки «меме»" ("Неизвестное поле «меме»." `T.isInfixOf` combined)
                assertBool "нет ошибки типа" ("Оператор «содержит»" `T.isInfixOf` combined)
                assertBool "нет ошибки даты" ("«за … дней»" `T.isInfixOf` combined)
    , testCase "синтаксическая ошибка содержит позицию" $
        let src = "подборка без кавычек\n"
         in case compileText "test.mix" src of
              Right _ -> assertFailure "ожидалась синтаксическая ошибка"
              Left (e : _) -> do
                cePos e @?= Just (1, 10)
                ceLineText e @?= "подборка без кавычек"
              Left [] -> assertFailure "список ошибок пуст"
    ]

------------------------------------------------------------------------------
-- Семантика: непротиворечивость условий
------------------------------------------------------------------------------

-- | Конфликт условия корневой группы с условием во вложенной
-- «любое»:-worlds разворачиваются, конфликт виден только в одной
-- из альтернатив.
nestedConflictSrc :: Text
nestedConflictSrc =
  T.unlines
    [ "подборка \"Тест\""
    , "где все {"
    , "  год > 2020"
    , "  любое {"
    , "    год < 2000"
    , "    жанр = \"rock\""
    , "  }"
    , "}"
    ]

-- | Одно и то же противоречие видно и во вложенной группе, и в
-- родительской: ошибка должна быть ровно одна.
dedupSrc :: Text
dedupSrc =
  T.unlines
    [ "подборка \"Тест\""
    , "где все {"
    , "  все {"
    , "    год > 2020"
    , "    год < 2000"
    , "  }"
    , "}"
    ]

-- | Противоречивая ветвь внутри «любое»: сама по себе она
-- невыполнима, поэтому ошибка есть.
anyDeadBranchSrc :: Text
anyDeadBranchSrc =
  T.unlines
    [ "подборка \"Тест\""
    , "где любое {"
    , "  все {"
    , "    год > 2020"
    , "    год < 2000"
    , "  }"
    , "  любимое"
    , "}"
    ]

-- | Четыре группы «любое» по 10 альтернатив: 10 000 миров больше
-- лимита 'maxWorlds', проверка собственных миров пропускается —
-- файл обязан остаться валидным (никаких ложных срабатываний).
explosionSrc :: Text
explosionSrc =
  T.unlines $
    ["подборка \"Тест\"", "где все {"]
      ++ concat
        [ ["  любое {"]
            ++ ["    год > " <> T.pack (show (2000 + n)) | n <- [1 .. 10 :: Int]]
            ++ ["  }"]
        | _ <- [1 .. 4 :: Int]
        ]
      ++ ["}"]

semanticTests :: TestTree
semanticTests =
  testGroup
    "Семантика: непротиворечивость"
    [ assertCompilesTo
        "любое: встречные границы валидны"
        (wrapGroup "любое" ["год > 2020", "год < 2000"])
        ( object
            [ "name" .= t "Тест"
            ,
              "any"
                .= [ object ["gt" .= object ["year" .= i 2020]]
                   , object ["lt" .= object ["year" .= i 2000]]
                   ]
            ]
        )
    , assertErrorContains
        "все: встречные границы - противоречие"
        (wrapGroup "все" ["год > 2020", "год < 2000"])
        "противоречит условию"
    , assertErrorPos
        "все: ошибка указывает на второе условие"
        (wrapGroup "все" ["год > 2020", "год < 2000"])
        (4, 3)
    , assertErrorContains
        "все: границы без разрыва между целыми"
        (wrapGroup "все" ["год > 1980", "год < 1981"])
        "противоречит условию"
    , assertErrorContains
        "все: диапазон не пересекается с >"
        (wrapGroup "все" ["год между 1980 и 1989", "год > 2020"])
        "противоречит условию"
    , assertErrorContains
        "все: два разных равенства текста"
        (wrapGroup "все" ["жанр = \"rock\"", "жанр = \"jazz\""])
        "противоречит условию"
    , assertErrorContains
        "все: = и != одного значения (текст)"
        (wrapGroup "все" ["жанр = \"rock\"", "жанр != \"rock\""])
        "противоречит условию"
    , assertErrorContains
        "все: = и != одного значения (число)"
        (wrapGroup "все" ["год = 2000", "год != 2000"])
        "противоречит условию"
    , assertErrorContains
        "все: диапазон из одного запрещённого числа"
        (wrapGroup "все" ["год между 2000 и 2000", "год != 2000"])
        "противоречит условию"
    , assertErrorContains
        "все: булево поле и противоположное значение"
        (wrapGroup "все" ["любимое", "любимое = нет"])
        "противоречит условию"
    , assertErrorContains
        "все: присутствует и отсутствует"
        (wrapGroup "все" ["replaygain присутствует", "replaygain отсутствует"])
        "противоречит условию"
    , assertErrorContains
        "все: содержит и не содержит одно и то же"
        (wrapGroup "все" ["жанр содержит \"rock\"", "жанр не содержит \"rock\""])
        "противоречит условию"
    , assertErrorContains
        "все: несравнимые префиксы"
        (wrapGroup "все" ["жанр начинается с \"ab\"", "жанр начинается с \"cd\""])
        "противоречит условию"
    , assertErrorContains
        "все: каждая пара возможна, совокупность - нет"
        (wrapGroup "все" ["год != 2000", "год != 2001", "год между 2000 и 2001"])
        "Условия поля «год» не могут выполняться одновременно"
    , assertErrorContains
        "пустое значение = (текст)"
        (wrap "название = \"\"")
        "требует непустое текстовое значение"
    , assertCompilesTo
        "пустое значение != допустимо"
        (wrap "название != \"\"")
        (object ["name" .= t "Тест", "all" .= [object ["isNot" .= object ["title" .= t ""]]]])
    , assertValid
        "разные поля не конфликтуют"
        (wrapGroup "все" ["год > 2020", "оценка < 2"])
    , assertErrorCount
        "вложенные группы: конфликт между подгруппами - одна ошибка"
        nestedConflictSrc
        1
    , assertErrorPos
        "вложенные группы: позиция на конфликтующем условии"
        nestedConflictSrc
        (5, 5)
    , assertErrorCount
        "дедупликация: вложенная группа не даёт второй ошибки"
        dedupSrc
        1
    , assertErrorCount
        "любое с невыполнимой ветвью - ошибка"
        anyDeadBranchSrc
        1
    , assertValid "взрыв комбинаций не даёт ложных ошибок" explosionSrc
    ]

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "Unit"
    [ fieldTests
    , operatorTests
    , groupTests
    , metadataTests
    , semanticTests
    , errorTests
    ]
