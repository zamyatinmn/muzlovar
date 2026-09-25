{-# LANGUAGE OverloadedStrings #-}

-- | Unit-тесты: каждое поле, каждый оператор, вложенные группы,
-- синтаксический сахар, ошибки типов, сортировка, необязательные
-- метаданные и точный формат сообщений об ошибках.
module UnitTests (unitTests) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List.NonEmpty as NE
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Nspeller.Ast
import Nspeller.Compiler (compileText, compileTextWithWarnings)
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

-- | Компиляция должна завершиться успехом и породить предупреждения,
-- содержащие данный фрагмент (ровно @expected@ штук). Предупреждения
-- не влияют на валидность: @Left@ здесь — провал теста.
assertWarns :: TestName -> Text -> Int -> Text -> TestTree
assertWarns name src expected needle = testCase name $
  case compileTextWithWarnings "test.mix" src of
    Left errs -> assertFailure ("ошибка компиляции:\n" <> T.unpack (renderAll errs))
    Right (_, warns) -> do
      assertEqual ("получено:\n" <> T.unpack (renderAll warns)) expected (length warns)
      let combined = T.intercalate "\n" (map renderCompileError warns)
      assertBool
        ( "нет предупреждения, содержащего \""
            <> T.unpack needle
            <> "\"; получено:\n"
            <> T.unpack combined
        )
        (needle `T.isInfixOf` combined)

-- | Компиляция успешна и не даёт ни одного предупреждения.
assertNoWarns :: TestName -> Text -> TestTree
assertNoWarns name src = assertWarns name src 0 ""

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
        (wrap "explicit = \"e\"")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["explicitstatus" .= t "e"]]]])
    , assertCompilesTo
        "explicit: Не определено (пустое значение набора)"
        (wrap "explicit = \"\"")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["explicitstatus" .= t ""]]]])
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
    , assertCompilesTo
        "добавлено не за N дней (notInTheLast)"
        (wrap "добавлено не за 30 дней")
        (object ["name" .= t "Тест", "all" .= [object ["notInTheLast" .= object ["dateadded" .= i 30]]]])
    , assertCompilesTo
        "последнее_прослушивание не за N дней"
        (wrap "последнее_прослушивание не за 60 дней")
        (object ["name" .= t "Тест", "all" .= [object ["notInTheLast" .= object ["lastplayed" .= i 60]]]])
    , assertCompilesTo
        "replaygain: дробное значение"
        (wrap "replaygain = -6.5")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["rgtrackgain" .= (-6.5 :: Scientific)]]]])
    , assertCompilesTo
        "replaygain: дробные границы «между»"
        (wrap "replaygain между -8 и -4.5")
        ( object
            [ "name" .= t "Тест"
            , "all"
                .= [ object
                       ["inTheRange" .= object ["rgtrackgain" .= ([-8, -4.5] :: [Scientific])]]
                   ]
            ]
        )
    , assertCompilesTo
        "replaygain: целое значение остаётся целым"
        (wrap "replaygain = -6")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["rgtrackgain" .= (-6 :: Scientific)]]]])
    , assertValid
        "explicit: текстовые операторы вне набора свободны"
        (wrap "explicit содержит \"e\"")
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
    , assertCompilesTo
        "логические поля в сортировке"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок {"
            , "  любимое возр"
            , "  обложка убыв"
            , "}"
            ]
        )
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["is" .= object ["loved" .= True]]]
            , "sort" .= t "loved,-hascoverart"
            ]
        )
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
    , assertValid
        "все: два равенства жанра (multivalue)"
        (wrapGroup "все" ["жанр = \"rock\"", "жанр = \"jazz\""])
    , assertValid
        "все: = и != жанра (multivalue)"
        (wrapGroup "все" ["жанр = \"rock\"", "жанр != \"rock\""])
    , assertErrorContains
        "все: два разных равенства текста"
        (wrapGroup "все" ["название = \"rock\"", "название = \"jazz\""])
        "противоречит условию"
    , assertErrorContains
        "все: = и != одного значения (текст)"
        (wrapGroup "все" ["название = \"rock\"", "название != \"rock\""])
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
-- Числовые границы: >=, <=, между, домен и предупреждения
------------------------------------------------------------------------------

-- | Пары «>=»/«<=» одного поля в «все» lowerингуются в один
-- нативный @inTheRange@ независимо от порядка следования.
boundaryTests :: TestTree
boundaryTests =
  testGroup
    "Числовые границы"
    [ assertCompilesTo
        ">= -> gt or is"
        (wrap "оценка >= 4")
        ( object
            [ "name" .= t "Тест"
            ,
              "all"
                .= [ object
                       [ "any"
                           .= [ object ["gt" .= object ["rating" .= i 4]]
                              , object ["is" .= object ["rating" .= i 4]]
                              ]
                       ]
                   ]
            ]
        )
    , assertCompilesTo
        ">= для года (без границ домена)"
        (wrap "год >= 2020")
        ( object
            [ "name" .= t "Тест"
            ,
              "all"
                .= [ object
                       [ "any"
                           .= [ object ["gt" .= object ["year" .= i 2020]]
                              , object ["is" .= object ["year" .= i 2020]]
                              ]
                       ]
                   ]
            ]
        )
    , assertCompilesTo
        "<= -> lt or is"
        (wrap "оценка <= 4")
        ( object
            [ "name" .= t "Тест"
            ,
              "all"
                .= [ object
                       [ "any"
                           .= [ object ["lt" .= object ["rating" .= i 4]]
                              , object ["is" .= object ["rating" .= i 4]]
                              ]
                       ]
                   ]
            ]
        )
    , assertCompilesTo
        "<= для прослушиваний"
        (wrap "прослушиваний <= 100")
        ( object
            [ "name" .= t "Тест"
            ,
              "all"
                .= [ object
                       [ "any"
                           .= [ object ["lt" .= object ["playcount" .= i 100]]
                              , object ["is" .= object ["playcount" .= i 100]]
                              ]
                       ]
                   ]
            ]
        )
    , assertCompilesTo
        "между для оценки"
        (wrap "оценка между 2 и 5")
        (object ["name" .= t "Тест", "all" .= [object ["inTheRange" .= object ["rating" .= [i 2, i 5]]]]])
    , assertCompilesTo
        "между для года"
        (wrap "год между 2000 и 2020")
        (object ["name" .= t "Тест", "all" .= [object ["inTheRange" .= object ["year" .= [i 2000, i 2020]]]]])
    , assertCompilesTo
        "пара >= и <= в все сливается в inTheRange"
        (wrapGroup "все" ["оценка >= 2", "оценка <= 5"])
        (object ["name" .= t "Тест", "all" .= [object ["inTheRange" .= object ["rating" .= [i 2, i 5]]]]])
    , assertCompilesTo
        "пара <= и >= в все сливается (обратный порядок)"
        (wrapGroup "все" ["оценка <= 5", "оценка >= 2"])
        (object ["name" .= t "Тест", "all" .= [object ["inTheRange" .= object ["rating" .= [i 2, i 5]]]]])
    , assertCompilesTo
        "в любое пара не сливается (разные ветви)"
        ( wrapGroup
            "любое"
            ["оценка >= 2", "оценка <= 5"]
        )
        ( object
            [ "name" .= t "Тест"
            ,
              "any"
                .= [ object ["any" .= [object ["gt" .= object ["rating" .= i 2]], object ["is" .= object ["rating" .= i 2]]]]
                   , object ["any" .= [object ["lt" .= object ["rating" .= i 5]], object ["is" .= object ["rating" .= i 5]]]]
                   ]
            ]
        )
    , testGroup
        "доменные ошибки"
        [ assertErrorContains
            "значение выше максимума"
            (wrap "оценка = 7")
            "вне допустимого диапазона"
        , assertErrorContains
            "значение ниже минимума"
            (wrap "прослушиваний >= -1")
            "вне допустимого диапазона"
        , assertErrorContains
            "отрицательный год вне диапазона"
            (wrap "год = -1")
            "вне допустимого диапазона поля «год»: не меньше 0"
        , assertErrorContains
            "год = -5: сравнение за нижней границей"
            (wrap "год < -5")
            "вне допустимого диапазона поля «год»: не меньше 0"
        , assertErrorContains
            "год < 0 не допускает ни одного значения"
            (wrap "год < 0")
            "Условие не может выполняться"
        , assertCompilesTo
            "год = 0 — нижняя граница допустима"
            (wrap "год = 0")
            (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["year" .= i 0]]]])
        , assertErrorContains
            "строгое сравнение упирается в максимум"
            (wrap "оценка > 5")
            "Условие не может выполняться"
        , assertErrorContains
            "строгое сравнение упирается в минимум"
            (wrap "прослушиваний < 0")
            "Условие не может выполняться"
        , assertErrorContains
            "диапазон выходит за верхнюю границу"
            (wrap "оценка между 0 и 6")
            "выходит за допустимые границы"
        , assertErrorContains
            "диапазон не пересекает домен"
            (wrap "оценка между 7 и 9")
            "не пересекает допустимый диапазон"
        , assertErrorContains
            "контрадикция >= и <="
            (wrapGroup "все" ["год >= 2020", "год <= 2000"])
            "не могут выполняться одновременно"
        ]
    , testGroup
        "типовые ошибки"
        [ assertErrorContains
            ">= на булевом поле"
            (wrap "любимое >= 3")
            "Оператор «>=» применим только к числовым полям."
        , assertErrorContains
            "<= на текстовом поле"
            (wrap "жанр <= 3")
            "Поле «жанр» имеет текстовый тип."
        , assertErrorContains
            "enum: значение вне набора"
            (wrap "explicit = \"explicit\"")
            "не входит в допустимые значения"
        , assertErrorContains
            "enum: перечень допустимых значений"
            (wrap "explicit != \"xyz\"")
            "«e» (Explicit), «c» (Clean), «» (Не определено)"
        , assertErrorContains
            "дробное значение целочисленного поля"
            (wrap "год = 1980.5")
            "Значение 1980.5 должно быть целым числом."
        , assertErrorContains
            "дробное значение: поле не поддерживает дробные"
            (wrap "год = 1980.5")
            "не поддерживает дробные значения"
        , assertErrorContains
            "дробная граница «между»"
            (wrap "год между 1980.5 и 1990")
            "должно быть целым числом"
        , assertErrorContains
            "не за N дней на текстовом поле"
            (wrap "жанр не за 5 дней")
            "Сравнение «не за … дней» применимо только к датовым полям"
        , assertErrorContains
            "не за N дней: поле имеет текстовый тип"
            (wrap "жанр не за 5 дней")
            "Поле «жанр» имеет текстовый тип."
        ]
    ]

------------------------------------------------------------------------------
-- Датовые условия: абсолютные даты ГГГГ-ММ-ДД
------------------------------------------------------------------------------

dateTests :: TestTree
dateTests =
  testGroup
    "Даты"
    [ assertCompilesTo
        "= дата"
        (wrap "последнее_прослушивание = 2020-01-01")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["lastplayed" .= t "2020-01-01"]]]])
    , assertCompilesTo
        "= дата в кавычках"
        (wrap "последнее_прослушивание = \"2020-01-01\"")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["lastplayed" .= t "2020-01-01"]]]])
    , assertCompilesTo
        "!= дата"
        (wrap "добавлено != 2020-01-01")
        (object ["name" .= t "Тест", "all" .= [object ["isNot" .= object ["dateadded" .= t "2020-01-01"]]]])
    , assertCompilesTo
        "> дата"
        (wrap "добавлено > 2020-01-01")
        (object ["name" .= t "Тест", "all" .= [object ["gt" .= object ["dateadded" .= t "2020-01-01"]]]])
    , assertCompilesTo
        "< дата"
        (wrap "последнее_прослушивание < 2020-01-01")
        (object ["name" .= t "Тест", "all" .= [object ["lt" .= object ["lastplayed" .= t "2020-01-01"]]]])
    , assertCompilesTo
        ">= дата -> gt или is"
        (wrap "добавлено >= 2020-01-01")
        ( object
            [ "name" .= t "Тест"
            ,
              "all"
                .= [ object
                       [ "any"
                           .= [ object ["gt" .= object ["dateadded" .= t "2020-01-01"]]
                              , object ["is" .= object ["dateadded" .= t "2020-01-01"]]
                              ]
                       ]
                   ]
            ]
        )
    , assertCompilesTo
        "<= дата -> lt или is"
        (wrap "последнее_прослушивание <= 2020-01-01")
        ( object
            [ "name" .= t "Тест"
            ,
              "all"
                .= [ object
                       [ "any"
                           .= [ object ["lt" .= object ["lastplayed" .= t "2020-01-01"]]
                              , object ["is" .= object ["lastplayed" .= t "2020-01-01"]]
                              ]
                       ]
                   ]
            ]
        )
    , assertCompilesTo
        "между датами -> inTheRange"
        (wrap "добавлено между 2024-01-01 и 2024-12-31")
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["inTheRange" .= object ["dateadded" .= [t "2024-01-01", t "2024-12-31"]]]]
            ]
        )
    , assertCompilesTo
        "до -> before"
        (wrap "последнее_прослушивание до 2024-06-01")
        (object ["name" .= t "Тест", "all" .= [object ["before" .= object ["lastplayed" .= t "2024-06-01"]]]])
    , assertCompilesTo
        "после -> after"
        (wrap "последнее_прослушивание после 2024-06-01")
        (object ["name" .= t "Тест", "all" .= [object ["after" .= object ["lastplayed" .= t "2024-06-01"]]]])
    , assertCompilesTo
        "пара >= и <= дат в все сливается в inTheRange"
        (wrapGroup "все" ["добавлено >= 2024-01-01", "добавлено <= 2024-12-31"])
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["inTheRange" .= object ["dateadded" .= [t "2024-01-01", t "2024-12-31"]]]]
            ]
        )
    , assertCompilesTo
        "в любое пара дат не сливается (разные ветви)"
        (wrapGroup "любое" ["добавлено >= 2024-01-01", "добавлено <= 2024-12-31"])
        ( object
            [ "name" .= t "Тест"
            ,
              "any"
                .= [ object ["any" .= [object ["gt" .= object ["dateadded" .= t "2024-01-01"]], object ["is" .= object ["dateadded" .= t "2024-01-01"]]]]
                   , object ["any" .= [object ["lt" .= object ["dateadded" .= t "2024-12-31"]], object ["is" .= object ["dateadded" .= t "2024-12-31"]]]]
                   ]
            ]
        )
    , assertValid
        "дата в кавычках после «до»"
        (wrap "последнее_прослушивание до \"2024-06-01\"")
    , assertValid
        "кавыченные даты в «между»"
        (wrap "добавлено между \"2024-01-01\" и \"2024-12-31\"")
    , testGroup
        "ошибки"
        [ assertErrorContains
            "до на числовом поле"
            (wrap "год до 2020-01-01")
            "Оператор «до» применим только к датовым полям."
        , assertErrorContains
            "до на числовом поле: тип поля"
            (wrap "год до 2020-01-01")
            "Поле «год» имеет числовой тип."
        , assertErrorContains
            "после на текстовом поле"
            (wrap "название после 2020-01-01")
            "Оператор «после» применим только к датовым полям."
        , assertErrorContains
            "до на логическом поле"
            (wrap "любимое до 2020-01-01")
            "Поле «любимое» имеет логический тип."
        , assertErrorContains
            "год между датами"
            (wrap "год между 2020-01-01 и 2020-12-31")
            "Оператор «между» применим только к датовым полям."
        , assertErrorContains
            "перевёрнутый датовый диапазон"
            (wrap "добавлено между 2020-01-01 и 2019-01-01")
            "Нижняя граница диапазона не может быть больше верхней: 2020-01-01 > 2019-01-01."
        , assertErrorContains
            "число вместо даты"
            (wrap "добавлено = 20200101")
            "ожидается дата в формате ГГГГ-ММ-ДД"
        , assertErrorContains
            "несуществующая дата"
            (wrap "добавлено = 2024-02-30")
            "несуществующая дата"
        , assertErrorContains
            "текстовый оператор на датовом поле"
            (wrap "последнее_прослушивание начинается с \"2020\"")
            "Оператор «начинается с» применим только к текстовым полям."
        , assertErrorContains
            "текстовый оператор на датовом поле: тип"
            (wrap "последнее_прослушивание начинается с \"2020\"")
            "Поле «последнее_прослушивание» имеет тип даты."
        ]
    , testGroup
        "семантика"
        [ assertErrorContains
            "до и после: противоречие"
            (wrapGroup "все" ["последнее_прослушивание до 2020-01-01", "последнее_прослушивание после 2021-01-01"])
            "противоречит условию"
        , assertErrorContains
            "= и != одной даты"
            (wrapGroup "все" ["добавлено = 2020-01-01", "добавлено != 2020-01-01"])
            "противоречит условию"
        , assertErrorContains
            "кавыченная дата участвует в противоречии"
            ( wrapGroup
                "все"
                ["последнее_прослушивание = \"2020-01-01\"", "последнее_прослушивание != 2020-01-01"]
            )
            "противоречит условию"
        , assertErrorContains
            "диапазон дат и «после» не пересекаются"
            ( wrapGroup
                "все"
                ["добавлено между 2019-01-01 и 2019-12-31", "добавлено после 2020-06-01"]
            )
            "противоречит условию"
        , assertValid
            "встречные даты в разных ветвях любого"
            (wrapGroup "любое" ["добавлено до 2020-01-01", "добавлено после 2021-01-01"])
        , assertValid
            "абсолютная дата и «за N дней» не противоречат"
            ( wrapGroup
                "все"
                ["последнее_прослушивание до 2020-01-01", "последнее_прослушивание за 30 дней"]
            )
        ]
    , testGroup
        "предупреждения"
        [ assertWarns
            "избыточная нижняя граница даты"
            (wrapGroup "все" ["последнее_прослушивание > 2020-01-01", "последнее_прослушивание > 2019-01-01"])
            1
            "избыточно"
        , assertNoWarns
            "абсолютная и относительная оси не дают ложных предупреждений"
            (wrapGroup "все" ["последнее_прослушивание > 2020-01-01", "последнее_прослушивание за 30 дней"])
        , assertNoWarns
            "диапазон дат не покрывает домен"
            (wrap "добавлено между 2024-01-01 и 2024-12-31")
        , assertNoWarns
            "границы дат в все не избыточны"
            (wrapGroup "все" ["добавлено >= 2024-01-01", "добавлено <= 2024-12-31"])
        ]
    ]

warningTests :: TestTree
warningTests =
  testGroup
    "Предупреждения"
    [ assertWarns
        "точный дубликат в все"
        (wrapGroup "все" ["год > 2020", "год > 2020"])
        1
        "дублирует"
    , assertWarns
        "более слабое условие избыточно"
        (wrapGroup "все" ["год > 2000", "год > 2020"])
        1
        "избыточно"
    , assertWarns
        "покрытие домена (>=)"
        (wrap "оценка >= 0")
        1
        "покрывает весь допустимый диапазон"
    , assertWarns
        "покрытие домена (между)"
        (wrap "оценка между 0 и 5")
        1
        "покрывает весь допустимый диапазон"
    , assertNoWarns
        "условия в разных ветвях любого не дубликаты"
        (wrapGroup "любое" ["год > 2020", "год > 2020"])
    , assertNoWarns
        "встречные границы в разных ветвях любого"
        (wrapGroup "любое" ["год > 2020", "год < 2000"])
    ]

------------------------------------------------------------------------------
-- Членство в подборке (inPlaylist / notInPlaylist)
------------------------------------------------------------------------------

playlistTests :: TestTree
playlistTests =
  testGroup
    "Членство в подборке"
    [ assertCompilesTo
        "в подборке id"
        (wrap "в подборке id \"abc-123\"")
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["inPlaylist" .= object ["id" .= t "abc-123"]]]
            ]
        )
    , assertCompilesTo
        "не в подборке id"
        (wrap "не в подборке id \"abc-123\"")
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["notInPlaylist" .= object ["id" .= t "abc-123"]]]
            ]
        )
    , assertCompilesTo
        "в подборке файл"
        (wrap "в подборке файл \"other.nsp\"")
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["inPlaylist" .= object ["path" .= t "other.nsp"]]]
            ]
        )
    , assertCompilesTo
        "не в подборке файл"
        (wrap "не в подборке файл \"../other.nsp\"")
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["notInPlaylist" .= object ["path" .= t "../other.nsp"]]]
            ]
        )
    , assertErrorContains
        "пустой идентификатор подборки"
        (wrap "в подборке id \"\"")
        "Идентификатор подборки не может быть пустым"
    , assertErrorContains
        "пустой путь к файлу подборки"
        (wrap "в подборке файл \"   \"")
        "Путь к файлу подборки не может быть пустым"
    , assertErrorContains
        "в подборке и не в подборке одной ссылки - противоречие"
        (wrapGroup "все" ["в подборке id \"x\"", "не в подборке id \"x\""])
        "противоречит условию"
    , assertValid
        "разные ссылки не противоречат друг другу"
        (wrapGroup "все" ["в подборке id \"x\"", "не в подборке id \"y\""])
    , assertValid
        "разные виды ссылки не противоречат друг другу"
        (wrapGroup "все" ["в подборке id \"x\"", "не в подборке файл \"x\""])
    , assertValid
        "булево условие непосредственно перед «не в подборке»"
        (wrapGroup "все" ["обложка", "не в подборке id \"x\""])
    , assertWarns
        "точная дубликация членства предупреждает один раз"
        (wrapGroup "все" ["в подборке id \"x\"", "в подборке id \"x\""])
        1
        "дублирует"
    , assertNoWarns
        "разные ссылки на подборки не дают предупреждений"
        (wrapGroup "все" ["в подборке id \"x\"", "в подборке id \"y\""])
    ]

------------------------------------------------------------------------------
-- Поля реестра (e2e: DSL → NSP)
------------------------------------------------------------------------------

-- | Полный e2e-набор для полей, добавленных расширением реестра:
-- каждый вид значения, проверка наличия, альбомные/артистные поля,
-- MusicBrainz ID, алиасы и отрицательные примеры.
registryFieldTests :: TestTree
registryFieldTests =
  testGroup
    "Поля реестра (e2e)"
    [ -- целочисленное поле
      assertCompilesTo
        "номер_трека (целое)"
        (wrap "номер_трека = 3")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["tracknumber" .= i 3]]]])
    , assertCompilesTo
        "номер_диска (целое, сравнение)"
        (wrap "номер_диска > 1")
        (object ["name" .= t "Тест", "all" .= [object ["gt" .= object ["discnumber" .= i 1]]]])
    , -- дробное поле
      assertCompilesTo
        "длительность (дробная)"
        (wrap "длительность > 200.5")
        ( object
            ["name" .= t "Тест", "all" .= [object ["gt" .= object ["duration" .= (200.5 :: Scientific)]]]]
        )
    , assertCompilesTo
        "replaygain_альбом (дробное, отрицательное)"
        (wrap "replaygain_альбом < -6.5")
        ( object
            ["name" .= t "Тест", "all" .= [object ["lt" .= object ["rgalbumgain" .= (-6.5 :: Scientific)]]]]
        )
    , -- текстовое поле
      assertCompilesTo
        "кодек (текст)"
        (wrap "кодек = \"MP3\"")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["codec" .= t "MP3"]]]])
    , assertCompilesTo
        "номер_в_каталоге (текст, содержит)"
        (wrap "номер_в_каталоге содержит \"ABC\"")
        ( object
            ["name" .= t "Тест", "all" .= [object ["contains" .= object ["catalognumber" .= t "ABC"]]]]
        )
    , -- проверка наличия
      assertCompilesTo
        "темп присутствует"
        (wrap "темп присутствует")
        (object ["name" .= t "Тест", "all" .= [object ["isPresent" .= object ["bpm" .= True]]]])
    , assertCompilesTo
        "битовая_глубина отсутствует"
        (wrap "битовая_глубина отсутствует")
        (object ["name" .= t "Тест", "all" .= [object ["isMissing" .= object ["bitdepth" .= True]]]])
    , assertCompilesTo
        "mbid_артиста отсутствует"
        (wrap "mbid_артиста отсутствует")
        ( object
            ["name" .= t "Тест", "all" .= [object ["isMissing" .= object ["mbz_artist_id" .= True]]]]
        )
    , -- логическое поле
      assertCompilesTo
        "сборник = да"
        (wrap "сборник = да")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["compilation" .= True]]]])
    , assertCompilesTo
        "файл_отсутствует (сахар)"
        (wrap "файл_отсутствует")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["missing" .= True]]]])
    , -- дата
      assertCompilesTo
        "дата_релиза после"
        (wrap "дата_релиза после 2020-01-01")
        ( object
            ["name" .= t "Тест", "all" .= [object ["after" .= object ["releasedate" .= t "2020-01-01"]]]]
        )
    , assertCompilesTo
        "дата_добавления_альбома за 30 дней"
        (wrap "дата_добавления_альбома за 30 дней")
        ( object
            ["name" .= t "Тест", "all" .= [object ["inTheLast" .= object ["albumdateadded" .= i 30]]]]
        )
    , assertCompilesTo
        "дата_любимого до"
        (wrap "дата_любимого до 2024-01-01")
        ( object
            ["name" .= t "Тест", "all" .= [object ["before" .= object ["dateloved" .= t "2024-01-01"]]]]
        )
    , -- альбомные и артистные поля
      assertCompilesTo
        "прослушиваний_альбома"
        (wrap "прослушиваний_альбома > 10")
        ( object
            ["name" .= t "Тест", "all" .= [object ["gt" .= object ["albumplaycount" .= i 10]]]]
        )
    , assertCompilesTo
        "длительность_альбома (дробная)"
        (wrap "длительность_альбома > 3600")
        ( object
            ["name" .= t "Тест", "all" .= [object ["gt" .= object ["albumduration" .= i 3600]]]]
        )
    , assertCompilesTo
        "любимый_артист = нет"
        (wrap "любимый_артист = нет")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["artistloved" .= False]]]])
    , assertCompilesTo
        "оценка_артиста = 5"
        (wrap "оценка_артиста = 5")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["artistrating" .= i 5]]]])
    , -- идентификаторы
      assertCompilesTo
        "mbid_альбома"
        (wrap "mbid_альбома = \"abc-123\"")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["mbz_album_id" .= t "abc-123"]]]])
    , assertCompilesTo
        "библиотека (library_id)"
        (wrap "библиотека = 2")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["library_id" .= i 2]]]])
    , -- алиасы Navidrome разрешаются в канонические имена
      assertCompilesTo
        "алиас replaygain_track_gain => rgtrackgain"
        (wrap "replaygain_track_gain > -6.5")
        ( object
            ["name" .= t "Тест", "all" .= [object ["gt" .= object ["rgtrackgain" .= (-6.5 :: Scientific)]]]]
        )
    , assertCompilesTo
        "NSP-имя поля разрешается в DSL"
        (wrap "tracknumber = 3")
        (object ["name" .= t "Тест", "all" .= [object ["is" .= object ["tracknumber" .= i 3]]]])
    , assertCompilesTo
        "алиас lastPlayed в сортировке => lastplayed"
        ( T.unlines
            [ "подборка \"Тест\""
            , "где все {"
            , "  любимое"
            , "}"
            , "порядок {"
            , "  lastPlayed убыв"
            , "}"
            ]
        )
        ( object
            [ "name" .= t "Тест"
            , "all" .= [object ["is" .= object ["loved" .= True]]]
            , "sort" .= t "-lastplayed"
            ]
        )
    , -- отрицательные примеры
      assertErrorContains
        "дробь на целочисленном поле"
        (wrap "номер_трека = 2.5")
        "Значение 2.5 должно быть целым числом."
    , assertErrorContains
        "проверка наличия на поле без признака"
        (wrap "библиотека присутствует")
        "не поддерживает проверку наличия"
    , assertErrorContains
        "текстовый оператор на числовом поле"
        (wrap "размер содержит \"икра\"")
        "применим только к текстовым полям"
    , assertErrorContains
        "значение вне границ рейтинга альбома"
        (wrap "оценка_альбома = 7")
        "вне допустимого диапазона"
    , assertErrorContains
        "условие вне неотрицательной области"
        (wrap "прослушиваний_альбома < 0")
        "не допускает ни одного допустимого значения"
    ]

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "Unit"
    [ fieldTests
    , registryFieldTests
    , operatorTests
    , groupTests
    , metadataTests
    , semanticTests
    , boundaryTests
    , dateTests
    , warningTests
    , errorTests
    , playlistTests
    ]
