{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Тесты реестра полей 'Nspeller.Fields.defaultRegistry': инварианты
-- записей (уникальность имён, соответствие вида и категории,
-- операторы, возможности) и то, что остальной код действительно
-- строится из реестра, а не из собственных списков полей.
--
-- Реестр — единственный источник правды, поэтому проверяется и его
-- связь с проекциями: 'Nspeller.Schema.fieldSchemas' должна быть
-- точным отражением записей, а поиск по имени — возвращать ту же
-- запись как под DSL-, так и под NSP-именем.
module RegistryTests (registryTests) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Type.Equality ((:~:) (..))
import Nspeller.Compiler (compileText)
import Nspeller.Fields
import Nspeller.Navidrome (encodeNsp)
import Nspeller.Schema (FieldSchema (..), OperatorSchema (..), fieldSchemas, operatorSchemas)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

------------------------------------------------------------------------------
-- Выборки из реестра
------------------------------------------------------------------------------

-- | Записи реестра в порядке DSL-палитры (упакованные).
registryEntries :: [SomeFieldSpec]
registryEntries = registrySpecs defaultRegistry

-- | DSL-имена всех полей реестра в порядке палитры.
registryDslNames :: [Text]
registryDslNames = [spDslName spec | SomeFieldSpec spec <- registryEntries]

-- | Все имена разрешения (DSL, NSP и алиасы) с ожидаемым результатом
-- поиска: статическое поле находит себя, псевдополя «подборка» нет.
lookupTable :: [(Text, Text, Bool)]
lookupTable =
  [ (name, spDslName spec, specHasCapability CapStatic spec)
  | SomeFieldSpec spec <- registryEntries
  , name <- specNames spec
  ]

-- | Подборка с одним условием из исходника DSL.
dslSource :: Text -> Text
dslSource body = T.unlines ["подборка \"Тест\"", "где все {", "  " <> body, "}"]

-- | Источник компилируется без ошибок (предупреждения не учитываются).
compiles :: Text -> Bool
compiles = isRight . compileText "test.mix"

------------------------------------------------------------------------------
-- Уникальность имён
------------------------------------------------------------------------------

-- | Имена всех записей уникальны в пределах реестра: DSL-имена,
-- NSP-имена и алиасы — по ним строится 'fieldByName'.
uniqueNames :: TestTree
uniqueNames = testCase "уникальность id/nspName/алиасов в реестре" $ do
  length registryEntries @?= 72
  let names = concat [specNames spec | SomeFieldSpec spec <- registryEntries]
      dslNames = [spDslName spec | SomeFieldSpec spec <- registryEntries]
      nspNames = [spNspName spec | SomeFieldSpec spec <- registryEntries]
      aliasNames = [a | SomeFieldSpec spec <- registryEntries, a <- spAliases spec]
  -- 72 поля × (DSL + NSP) + 7 алиасов
  length names @?= 151
  sort (nub names) @?= sort names
  -- DSL-имена и NSP-имена сами по себе уникальны, а алиасы не
  -- пересекаются ни с ними, ни друг с другом
  length (nub dslNames) @?= 72
  length (nub nspNames) @?= 72
  length aliasNames @?= 7
  sort (nub (dslNames <> nspNames <> aliasNames)) @?= sort names
  -- алиасы — только задокументированные альтернативные написания
  sort aliasNames
    @?= sort
      [ "replaygain_track_gain"
      , "replaygain_track_peak"
      , "replaygain_album_gain"
      , "replaygain_album_peak"
      , "lastPlayed"
      , "playCount"
      , "dateLoved"
      ]

-- | Полный ожидаемый набор статических NSP-имён (71 поле): реестр
-- покрывает таблицу Fields документации Navidrome и не содержит
-- лишних статических полей.
expectedNspNames :: TestTree
expectedNspNames = testCase "полный набор статических NSP-имён" $ do
  let nspNames =
        [ spNspName spec
        | SomeFieldSpec spec <- registryEntries
        , specHasCapability CapStatic spec
        ]
      expected =
        [ "album"
        , "albumcomment"
        , "albumdateloved"
        , "albumdateadded"
        , "albumdatemodified"
        , "albumdaterated"
        , "albumduration"
        , "albumlastplayed"
        , "albumloved"
        , "albumplaycount"
        , "albumrating"
        , "albumsongcount"
        , "albumsize"
        , "artistdateloved"
        , "artistdaterated"
        , "artistlastplayed"
        , "artistloved"
        , "artistrating"
        , "artistplaycount"
        , "averagerating"
        , "bitdepth"
        , "bitrate"
        , "bpm"
        , "catalognumber"
        , "channels"
        , "codec"
        , "compilation"
        , "comment"
        , "date"
        , "dateadded"
        , "dateloved"
        , "datemodified"
        , "daterated"
        , "discnumber"
        , "discsubtitle"
        , "duration"
        , "explicitstatus"
        , "filepath"
        , "filetype"
        , "genre"
        , "hascoverart"
        , "lastplayed"
        , "library_id"
        , "loved"
        , "lyrics"
        , "mbz_album_artist_id"
        , "mbz_album_id"
        , "mbz_artist_id"
        , "mbz_recording_id"
        , "mbz_release_group_id"
        , "mbz_release_track_id"
        , "missing"
        , "originaldate"
        , "originalyear"
        , "playcount"
        , "rating"
        , "releaseyear"
        , "releasedate"
        , "rgalbumgain"
        , "rgalbumpeak"
        , "rgtrackgain"
        , "rgtrackpeak"
        , "samplerate"
        , "size"
        , "sortalbum"
        , "sortalbumartist"
        , "sortartist"
        , "sorttitle"
        , "title"
        , "tracknumber"
        , "year"
        ]
  length nspNames @?= 71
  sort nspNames @?= sort expected
  -- псевдополе «подборка» статическим полем не является
  assertBool "inPlaylist не должно быть статическим полем" ("inPlaylist" `notElem` nspNames)

------------------------------------------------------------------------------
-- Вид и категория значения
---------------------------------------------------------------------------------

-- | Вид каждой записи согласован с её категорией значения:
-- 'kindValueType' 'spKind' = 'spCategory' — иначе сообщения об
-- ошибках и JSON-коды разошлись бы с тем, что принимает валидация.
kindCategoryAgreement :: TestTree
kindCategoryAgreement = testCase "вид поля согласован с категорией значения" $
  forM_ registryEntries $ \entry -> case entry of
    SomeFieldSpec spec ->
      kindValueType (spKind spec) @?= spCategory spec

-- | Пять видов покрывают реестр ровно один раз: разбиение
-- 'fieldsOfKind' не теряет и не дублирует поля.
kindPartition :: TestTree
kindPartition = testCase "fieldsOfKind покрывает все записи реестра" $ do
  let textNames = map fieldDslName (fieldsOfKind KindText)
      numberNames = map fieldDslName (fieldsOfKind KindNumber)
      boolNames = map fieldDslName (fieldsOfKind KindBool)
      dateNames = map fieldDslName (fieldsOfKind KindDate)
      refNames = map fieldDslName (fieldsOfKind KindPlaylistRef)
      covered = textNames <> numberNames <> boolNames <> dateNames <> refNames
  -- ожидаемое распределение: 22 текстовых, 27 числовых (в том числе
  -- 7 дробных), 6 логических, 16 датовых и псевдополе «подборка»
  (length textNames, length numberNames, length boolNames, length dateNames, length refNames)
    @?= (22, 27, 6, 16, 1)
  sort covered @?= sort registryDslNames
  length (nub covered) @?= 72
  -- дробными (spIntegral = False) объявлены ровно ReplayGain-поля,
  -- длительности и средняя оценка
  let nonIntegral =
        [ fieldDslName r
        | r <- fieldsOfKind KindNumber
        , not (fieldIsIntegral r)
        ]
  sort nonIntegral
    @?= sort
      [ "replaygain"
      , "replaygain_пик"
      , "replaygain_альбом"
      , "replaygain_пик_альбом"
      , "длительность"
      , "длительность_альбома"
      , "средняя_оценка"
      ]

------------------------------------------------------------------------------
-- Поиск по имени
------------------------------------------------------------------------------

-- | 'fieldByName' находит каждую статическую запись и под DSL-, и под
-- NSP-именем (оба имени ведут к одной и той же записи, равенство —
-- по DSL-имени); псевдополя «подборка» и неизвестные имена не
-- разрешаются.
lookupByName :: TestTree
lookupByName = testCase "поиск по имени: DSL и NSP" $ do
  forM_ registryEntries $ \entry -> case entry of
    SomeFieldSpec spec -> forM_ (specNames spec) $ \name ->
      if specHasCapability CapStatic spec
        then case fieldByName name of
          Nothing -> assertFailure ("не найдено поле: " <> T.unpack name)
          Just (SomeField r) -> do
            fieldDslName r @?= spDslName spec
            -- вид найденной ссылки совпадает с видом записи:
            -- kindEq даёт свидетельство типа, а не только Bool
            assertBool
              ("kindEq не проходит для " <> T.unpack name)
              (isJust (kindEq (fieldKind r) (spKind spec)))
        else fieldByName name @?= Nothing
  -- DSL-имя, NSP-имя и алиас одного поля дают равные ссылки
  fieldByName "название" @?= fieldByName "title"
  fieldByName "последнее_прослушивание" @?= fieldByName "lastplayed"
  fieldByName "последнее_прослушивание" @?= fieldByName "lastPlayed"
  fieldByName "replaygain" @?= fieldByName "replaygain_track_gain"
  fieldByName "прослушиваний" @?= fieldByName "playCount"
  fieldByName "дата_любимого" @?= fieldByName "dateLoved"
  -- псевдополе и неизвестное имя
  fieldByName "подборка" @?= Nothing
  fieldByName "inPlaylist" @?= Nothing
  fieldByName "нет_такого_поля" @?= Nothing

------------------------------------------------------------------------------
-- Алиасы
------------------------------------------------------------------------------

-- | Каждый алиас ('spAliases') ведёт к своей записи, а для
-- сортируемого поля канонизируется в то же DSL-имя, что и основные
-- имена.
aliasResolution :: TestTree
aliasResolution = testCase "алиасы разрешаются в свою запись" $
  forM_ registryEntries $ \entry -> case entry of
    SomeFieldSpec spec -> forM_ (spAliases spec) $ \alias -> do
      case fieldByName alias of
        Nothing -> assertFailure ("алиас не разрешился: " <> T.unpack alias)
        Just (SomeField r) -> fieldDslName r @?= spDslName spec
      sortFieldByName alias
        @?= ( if spSortable spec
                then Just (spDslName spec)
                else Nothing
            )

------------------------------------------------------------------------------
-- Поиск без учёта регистра
------------------------------------------------------------------------------

-- | Варианты написания одного имени: как в реестре, верхний регистр,
-- нижний, с заглавной буквы и с строчной первой буквой (покрывает и
-- camelCase-алиасы вроде @lastPlayed@).
caseVariants :: Text -> [Text]
caseVariants name =
  [ name
  , T.toUpper name
  , T.toLower name
  , T.toUpper (T.take 1 name) <> T.drop 1 name
  , T.toLower (T.take 1 name) <> T.drop 1 name
  ]

-- | Как Navidrome 'LookupField' ('strings.ToLower'): любое имя
-- ('spDslName'/'spNspName'/'spAliases') разрешается без учёта
-- регистра и ведёт в ту же запись, а результат канонизации
-- ('sortFieldByName', 'sortFieldName') от регистра не зависит.
caseInsensitiveLookup :: TestTree
caseInsensitiveLookup = testCase "поиск имени без учёта регистра" $ do
  forM_ registryEntries $ \entry -> case entry of
    SomeFieldSpec spec -> forM_ (specNames spec) $ \name ->
      forM_ (caseVariants name) $ \v -> do
        -- та же запись (или Nothing у псевдополя «подборка»)
        fieldByName v @?= fieldByName name
        case fieldByName v of
          Just (SomeField r) -> fieldDslName r @?= spDslName spec
          Nothing -> pure ()
        -- сортировка: канонические имена не зависят от регистра
        sortFieldByName v @?= sortFieldByName name
        sortFieldName v @?= sortFieldName name
  -- явные примеры: DSL, NSP и алиас в другом регистре
  fieldByName "TITLE" @?= fieldByName "title"
  fieldByName "Название" @?= fieldByName "название"
  fieldByName "PLAYCOUNT" @?= fieldByName "прослушиваний"
  fieldByName "LastPlayed" @?= fieldByName "последнее_прослушивание"
  sortFieldByName "TITLE" @?= Just "название"
  sortFieldByName "PlayCount" @?= Just "прослушиваний"
  sortFieldName "TITLE" @?= "title"
  sortFieldName "PlayCount" @?= "playcount"
  -- неизвестные имена по-прежнему не разрешаются
  fieldByName "МЕМЕ" @?= Nothing
  sortFieldByName "МЕМЕ" @?= Nothing

-- | Регистронезависимость не создаёт неоднозначности: нижний регистр
-- имени не может принадлежать двум разным записям, иначе результат
-- 'specByName' зависел бы от порядка реестра. Совпадение в нижнем
-- регистре внутри одной записи допустимо (camelCase-алиас @dateLoved@
-- — это то же имя, что и NSP @dateloved@).
caseFoldedNamesDisjoint :: TestTree
caseFoldedNamesDisjoint = testCase "lowercase-имена разных записей не пересекаются" $ do
  let ownersOf low =
        nub
          [ spDslName spec
          | SomeFieldSpec spec <- registryEntries
          , n <- specNames spec
          , T.toLower n == low
          ]
      lows =
        nub
          [ T.toLower n
          | SomeFieldSpec spec <- registryEntries
          , n <- specNames spec
          ]
      ambiguous = [(low, ownersOf low) | low <- lows, length (ownersOf low) > 1]
  ambiguous @?= []
  -- 151 имя; три camelCase-алиаса (dateLoved, lastPlayed, playCount)
  -- совпадают в нижнем регистре с NSP-именем своей же записи
  length lows @?= 148

-- | Компиляция исходника с другим регистром имён: @.nsp@ всегда
-- канонический — NSP-имена реестра строчными, исходное написание не
-- протекает ни в условия, ни в сортировку.
caseInsensitiveCompile :: TestTree
caseInsensitiveCompile = testCase "канонический .nsp при другом регистре имён" $ do
  let src =
        T.unlines
          [ "подборка \"Регистр\""
          , "где все {"
          , "  Название = \"Тест\""
          , "  TITLE содержит \"Тест\""
          , "  ПРОСЛУШИВАНИЙ > 5"
          , "  ЛЮБИМОЕ"
          , "}"
          , "порядок { TITLE возр }"
          ]
  case compileText "test.mix" src of
    Left es -> assertFailure ("ошибки компиляции: " <> show es)
    Right nsp -> do
      let out = TE.decodeUtf8 (LBS.toStrict (encodeNsp nsp))
          check needle present =
            assertBool
              ( (if present then "нет «" else "протекло «")
                  <> T.unpack needle
                  <> "» в .nsp: "
                  <> T.unpack out
              )
              (needle `T.isInfixOf` out == present)
      check "\"title\": \"Тест\"" True
      check "\"contains\": {" True
      check "\"playcount\": 5" True
      check "\"loved\": true" True
      check "\"sort\": \"title\"" True
      check "Название" False
      check "TITLE" False
      check "ПРОСЛУШИВАНИЙ" False
      check "ЛЮБИМОЕ" False

------------------------------------------------------------------------------
-- Схема как проекция реестра
------------------------------------------------------------------------------

-- | 'Nspeller.Schema.fieldSchemas' построена из тех же записей, что и
-- реестр: идентификаторы, подписи, группы, категории, признаки,
-- ограничения, enum и виды ссылки совпадают запись в запись.
schemaIsRegistryProjection :: TestTree
schemaIsRegistryProjection = testCase "схема = проекция реестра" $ do
  length fieldSchemas @?= length registryEntries
  forM_ (zip fieldSchemas registryEntries) $ \pair -> case pair of
    (fs, SomeFieldSpec spec) -> do
      fsId fs @?= spDslName spec
      fsNspName fs @?= spNspName spec
      fsTitle fs @?= spTitle spec
      fsGroup fs @?= spGroup spec
      fsValueType fs @?= spCategory spec
      fsSortable fs @?= spSortable spec
      fsPresenceCapable fs @?= spPresence spec
      fsIntegral fs @?= spIntegral spec
      fsValueVariants fs @?= spValueVariants spec
      fsEnum fs @?= spEnum spec
      fsRefKinds fs @?= spRefKinds spec
      fsMin fs @?= (ncMin =<< spConstraints spec)
      fsMax fs @?= (ncMax =<< spConstraints spec)
      fsStep fs @?= (ncStep =<< spConstraints spec)
      -- операторы схемы — та же последовательность, что и у записи
      -- (имена и слоты проверяются на уровне JSON в SchemaTests)
      length (fsOperators fs) @?= length (spOperators spec)

-- | Идентификаторы полного каталога операторов.
catalogOpIds :: [Text]
catalogOpIds = map osId operatorSchemas

------------------------------------------------------------------------------
-- Операторы
------------------------------------------------------------------------------

-- | Операторы каждой записи — операторы её вида ('operatorsForKind'),
-- и каждый идентификатор есть в каталоге 'operatorSchemas':
-- совместимость «оператор × вид» не расходится с документацией.
operatorsMatchKind :: TestTree
operatorsMatchKind = testCase "операторы поля = операторы вида" $
  forM_ registryEntries $ \entry -> case entry of
    SomeFieldSpec spec -> do
      spOperators spec @?= operatorsForKind (spKind spec)
      forM_ (spOperators spec) $ \op ->
        assertBool
          ("оператор вне каталога: " <> T.unpack (unOpId (fopId op)))
          (unOpId (fopId op) `elem` catalogOpIds)

------------------------------------------------------------------------------
-- Возможности
------------------------------------------------------------------------------

-- | Возможности записей: персональные поля — ровно те, что ждёт
-- @/api/schema@; цель сахара «не звучало N дней» — одно датовое
-- статическое поле; 'CapStatic' есть у каждого поля, кроме
-- псевдополя «подборка»; каждая возможность указана не более одного
-- раза и у каждой возможности есть носитель.
capabilities :: TestTree
capabilities = testGroup
  "Возможности полей"
  [ testCase "персональные поля" $
      capabilityFieldNames CapPersonal
        @?= [ "любимое"
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
  , testCase "персональных полей ровно 18" $
      length (capabilityFieldNames CapPersonal) @?= 18
  , testCase "цель сахара «не звучало N дней» — датовое поле" $
      case fieldByCapability CapNotPlayed of
        Nothing -> assertFailure "ни одно поле не имеет CapNotPlayed"
        Just (SomeField r) -> do
          fieldDslName r @?= "последнее_прослушивание"
          assertBool "цель CapNotPlayed не статическое поле"
            (fieldHasCapability CapStatic r)
          case kindEq (fieldKind r) KindDate of
            Just Refl -> pure ()
            Nothing -> assertFailure "цель CapNotPlayed не датового вида"
  , testCase "каждая возможность имеет носителя" $
      forM_ [minBound .. maxBound] $ \cap ->
        assertBool
          ("нет поля с возможностью " <> show cap)
          (isJust (fieldByCapability cap))
  , testCase "CapStatic ⟺ не псевдополе ссылки" $
      forM_ registryEntries $ \entry -> case entry of
        SomeFieldSpec spec -> do
          specHasCapability CapStatic spec
            @?= (spCategory spec /= PlaylistRefType)
          -- возможности без повторов
          let caps = spCapabilities spec
          length (nub caps) @?= length caps
          -- каждая возможность из объявленного набора
          forM_ caps $ \cap ->
            assertBool ("неизвестная возможность " <> show cap)
              (cap `elem` ([minBound .. maxBound] :: [FieldCapability]))
  ]

------------------------------------------------------------------------------
-- Сортировка
------------------------------------------------------------------------------

-- | Сортировка читается из реестра: канонизация DSL ↔ NSP для
-- сортируемых полей, 'Nothing' для несортируемых и неизвестных имён,
-- ровно 71 сортируемое поле (всё, кроме «подборка»).
sortingFromRegistry :: TestTree
sortingFromRegistry = testCase "сортировка из реестра" $ do
  let dslOf spec = (spDslName spec, spNspName spec, spSortable spec)
      entries = [dslOf spec | SomeFieldSpec spec <- registryEntries]
      sortable = [(d, n) | (d, n, True) <- entries]
      notSortable = [(d, n) | (d, n, False) <- entries]
  length sortable @?= 71
  length notSortable @?= 1
  forM_ sortable $ \(dsl, nsp) -> do
    sortFieldByName dsl @?= Just dsl
    sortFieldByName nsp @?= Just dsl
    sortFieldName dsl @?= nsp
  forM_ notSortable $ \(dsl, nsp) -> do
    sortFieldByName dsl @?= Nothing
    sortFieldByName nsp @?= Nothing
  sortFieldByName "нет_такого_поля" @?= Nothing
  sortFieldName "нет_такого_поля" @?= "нет_такого_поля"

------------------------------------------------------------------------------
-- Инварианты признаков на уровне компиляции
------------------------------------------------------------------------------

-- | «поле присутствует»/«поле отсутствует» компилируется ровно тогда,
-- когда у записи стоит 'spPresence': признак схемы и валидация не
-- расходятся ни в одну сторону для любого поля реестра.
presenceInvariant :: TestTree
presenceInvariant = testCase "«присутствует»/«отсутствует» по признаку spPresence" $
  forM_ registryEntries $ \entry -> case entry of
    SomeFieldSpec spec
      | not (specHasCapability CapStatic spec) -> pure ()
      | otherwise ->
          forM_ ["присутствует", "отсутствует"] $ \op -> do
            let name = spDslName spec
            assertEqual
              (T.unpack (name <> " " <> op))
              (spPresence spec)
              (compiles (dslSource (name <> " " <> op)))

-- | «порядок { поле возр }» компилируется ровно тогда, когда у записи
-- 'spSortable'.
sortableInvariant :: TestTree
sortableInvariant = testCase "«порядок { поле возр }» по признаку spSortable" $
  forM_ registryEntries $ \entry -> case entry of
    SomeFieldSpec spec -> do
      let name = spDslName spec
          src =
            T.unlines
              [ "подборка \"Тест\""
              , "где все {"
              , "  год = 2000"
              , "}"
              , "порядок { " <> name <> " возр }"
              ]
      assertEqual (T.unpack name) (spSortable spec) (compiles src)

------------------------------------------------------------------------------
-- Show/Eq ссылок
------------------------------------------------------------------------------

-- | 'Show' ссылки печатает 'fieldRef' с DSL-именем — вид, который
-- сохраняет 'Nspeller.Ast.ValidCond' (имя показывается так же, как
-- @show@ показывает 'Text', то есть с экранированием); 'SomeField'
-- сравнивается по DSL-имени.
refShowEq :: TestTree
refShowEq = testCase "Show/Eq ссылок на поле" $ do
  case fieldByName "название" of
    Nothing -> assertFailure "поле «название» не найдено"
    Just ref -> show ref @?= "fieldRef " <> show ("название" :: Text)
  case fieldByName "подборка" of
    Just _ -> assertFailure "псевдополе не должно разрешаться"
    Nothing -> pure ()

------------------------------------------------------------------------------
-- Свойства (QuickCheck)
------------------------------------------------------------------------------

-- | Имя из 'specNames' разрешается ровно тогда, когда запись
-- статическая, и результат — та же запись (по DSL-имени).
prop_lookupRoundTrip :: Property
prop_lookupRoundTrip = forAll (elements lookupTable) $ \(name, dsl, isStatic) ->
  case fieldByName name of
    Just (SomeField r) ->
      counterexample ("имя " <> T.unpack name <> " не привело к записи " <> T.unpack dsl)
        (isStatic && fieldDslName r == dsl)
    Nothing ->
      counterexample ("имя " <> T.unpack name <> " не разрешилось, ожидалось: " <> show isStatic)
        (not isStatic)

-- | Для любой записи реестра операторы равны операторам её вида, а
-- категория значения — категории вида.
prop_operatorsMatchKind :: Property
prop_operatorsMatchKind =
  forAll (elements [0 .. length registryEntries - 1]) $ \i ->
    case registryEntries !! i of
      SomeFieldSpec spec ->
        counterexample (T.unpack (spDslName spec))
          ( spOperators spec == operatorsForKind (spKind spec)
              && kindValueType (spKind spec) == spCategory spec
          )

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

registryTests :: TestTree
registryTests =
  testGroup
    "Registry"
    [ uniqueNames
    , expectedNspNames
    , kindCategoryAgreement
    , kindPartition
    , lookupByName
    , aliasResolution
    , caseInsensitiveLookup
    , caseFoldedNamesDisjoint
    , caseInsensitiveCompile
    , schemaIsRegistryProjection
    , operatorsMatchKind
    , capabilities
    , sortingFromRegistry
    , presenceInvariant
    , sortableInvariant
    , refShowEq
    , testProperty "имя поля разрешается в ту же запись" prop_lookupRoundTrip
    , testProperty "операторы поля согласованы с видом" prop_operatorsMatchKind
    ]
