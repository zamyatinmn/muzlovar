{-# LANGUAGE OverloadedStrings #-}

-- | Интеграционные тесты файлового хранилища Muzlovar: публикация,
-- конфликты, безопасность slug, статусы managed/external/broken,
-- корзина (удаление, восстановление, очистка) и защита от
-- символических ссылок.
module StoreTests (withTempStore, storeTests) where

import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (forM_, when)
import Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nspeller.Muzlovar.Store
import Nspeller.Muzlovar.Types
import System.Directory
  ( createDirectory
  , createFileLink
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , getTemporaryDirectory
  , listDirectory
  , removeDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>), takeExtension, takeFileName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

------------------------------------------------------------------------------
-- Временное хранилище
------------------------------------------------------------------------------

-- | Временный каталог и три каталога хранилища внутри него;
-- удаляется всегда, в том числе при исключении.
withTempStore :: String -> (StoreConfig -> IO a) -> IO a
withTempStore label act =
  bracket
    (createBase label)
    cleanupBase
    (\base -> setupCfg base >>= act)

-- | Уникальный каталог во временном каталоге системы.
createBase :: String -> IO FilePath
createBase label = do
  tmp <- getTemporaryDirectory
  let base = tmp </> ("muzlovar-test-" ++ label)
  go base (0 :: Int)
  where
    go :: FilePath -> Int -> IO FilePath
    go base i = do
      let dir = if i == 0 then base else base <> "-" <> show i
      exists <- doesPathExist dir
      if exists
        then go base (i + 1)
        else do
          createDirectory dir
          pure dir

cleanupBase :: FilePath -> IO ()
cleanupBase dir = do
  exists <- doesDirectoryExist dir
  when exists (removeDirectoryRecursive dir)

setupCfg :: FilePath -> IO StoreConfig
setupCfg base = do
  let cfg =
        StoreConfig
          { scRulesDir = base </> "rules"
          , scPlaylistsDir = base </> "playlists"
          , scTrashDir = base </> "trash"
          }
  r <- ensureStoreDirs cfg
  case r of
    Left e ->
      assertFailure ("не удалось создать каталоги: " <> T.unpack (storeErrorMessage e))
    Right () -> pure cfg

------------------------------------------------------------------------------
-- Пути и помощники
------------------------------------------------------------------------------

mixPath :: StoreConfig -> Text -> FilePath
mixPath cfg slug = scRulesDir cfg </> (T.unpack slug ++ ".mix")

nspFile :: StoreConfig -> Text -> FilePath
nspFile cfg slug = scPlaylistsDir cfg </> (T.unpack slug ++ ".nsp")

publishOk :: StoreConfig -> Text -> PlaylistDto -> IO PlaylistDetail
publishOk cfg slug dto = do
  r <- publishPlaylist cfg slug False dto
  case r of
    Left e ->
      assertFailure ("публикация не удалась: " <> T.unpack (storeErrorMessage e))
    Right d -> pure d

listOk :: StoreConfig -> IO [PlaylistEntry]
listOk cfg = do
  r <- listPlaylists cfg
  case r of
    Left e -> assertFailure ("listPlaylists: " <> T.unpack (storeErrorMessage e))
    Right xs -> pure xs

readOk :: StoreConfig -> Text -> IO PlaylistDetail
readOk cfg slug = do
  r <- readPlaylist cfg slug
  case r of
    Left e -> assertFailure ("readPlaylist: " <> T.unpack (storeErrorMessage e))
    Right d -> pure d

deleteOk :: StoreConfig -> Text -> IO Text
deleteOk cfg slug = do
  r <- deletePlaylistFiles cfg slug
  case r of
    Left e ->
      assertFailure ("deletePlaylistFiles: " <> T.unpack (storeErrorMessage e))
    Right tid -> pure tid

trashOk :: StoreConfig -> IO [TrashEntry]
trashOk cfg = do
  r <- listTrash cfg
  case r of
    Left e -> assertFailure ("listTrash: " <> T.unpack (storeErrorMessage e))
    Right xs -> pure xs

-- | Найти запись списка по slug (ровно одну).
entryFor :: [PlaylistEntry] -> Text -> IO PlaylistEntry
entryFor entries slug = case [e | e <- entries, peSlug e == slug] of
  [e] -> pure e
  other ->
    assertFailure
      ("ожидалась ровно одна запись «" <> T.unpack slug <> "», получено: " <> show other)

-- | Байты @.nsp@ для валидного DTO.
compileNsp :: PlaylistDto -> IO BS.ByteString
compileNsp dto = case compilePlaylistDto dto of
  Left es -> assertFailure ("compilePlaylistDto: " <> show es)
  Right c -> pure (LBS.toStrict (cmpNsp c))

-- | Файлы только с ожидаемыми именами: @.mix@/@.nsp@ и persisted
-- state публикации (нет остатков @.tmp@/@.bak@).
onlyFinalFiles :: StoreConfig -> [FilePath] -> Bool
onlyFinalFiles cfg = all ok
  where
    ok f =
      f == takeFileName (stateFilePath cfg)
        || takeExtension f `elem` [".mix", ".nsp"]

-- | Проверить существование/отсутствие файлов: @(путь, должен существовать)@.
assertFiles :: [(FilePath, Bool)] -> Assertion
assertFiles = mapM_ check
  where
    check (p, want) = do
      ex <- doesFileExist p
      assertBool
        (p <> ": ожидалось exist=" <> show want <> ", фактически " <> show ex)
        (ex == want)

-- | В каталогах не осталось служебных файлов (@.tmp@/@.bak@), а есть
-- только @.mix@/@.nsp@ и persisted state публикации.
assertNoLeftovers :: StoreConfig -> Assertion
assertNoLeftovers cfg = do
  rules <- listDirectory (scRulesDir cfg)
  playlists <- listDirectory (scPlaylistsDir cfg)
  assertBool
    ("лишние файлы: " <> show (rules ++ playlists))
    (onlyFinalFiles cfg rules && onlyFinalFiles cfg playlists)

------------------------------------------------------------------------------
-- Тестовая подборка
------------------------------------------------------------------------------

sampleDto :: PlaylistDto
sampleDto =
  PlaylistDto
    { pdName = "Любимые треки"
    , pdDescription = Just "Описание"
    , pdPublic = True
    , pdRoot =
        GroupDto
          "all"
          [ ItemCond (CondDto "любимое" "bare" Nothing)
          , ItemCond (CondDto "год" "between" (Just (toJSON ([1980, 1989] :: [Integer]))))
          ]
    , pdSort = Just SortRandomDto
    , pdLimit = Just 50
    }

-- | Slug из названия тестовой подборки.
sampleSlug :: Text
sampleSlug = slugFromName (pdName sampleDto)

------------------------------------------------------------------------------
-- Переименование опубликованной подборки: помощники
------------------------------------------------------------------------------

-- | Новое название для тестов переименования: slug всегда вычисляется
-- из названия, как это делает PUT /api/playlists/:slug.
renameNewName :: Text
renameNewName = "Новое Название"

-- | Slug, который получит подборка после переименования.
renameNewSlug :: Text
renameNewSlug = slugFromName renameNewName

-- | DTO с заданным названием (slug — следствие названия).
dtoNamed :: Text -> PlaylistDto
dtoNamed name = sampleDto {pdName = name}

-- | Публикация с указанием прежнего slug (PUT-семантика); неудача —
-- падение теста с сообщением ошибки.
publishFromOk ::
  StoreConfig -> Maybe Text -> Text -> Bool -> PlaylistDto -> IO PlaylistDetail
publishFromOk cfg mPrev slug overwrite dto = do
  r <- publishPlaylistFrom cfg mPrev slug overwrite dto
  case r of
    Left e ->
      assertFailure ("публикация не удалась: " <> T.unpack (storeErrorMessage e))
    Right d -> pure d

-- | Ожидается отказ cleanup_blocked именно по этому файлу: публикация
-- отменена до записи.
expectBlocked :: FilePath -> IO (Either StoreError a) -> Assertion
expectBlocked want act = do
  r <- act
  case r of
    Left (StoreCleanupBlocked p _) -> p @?= want
    Left e ->
      assertFailure ("ожидался StoreCleanupBlocked, получено: " <> show e)
    Right _ -> assertFailure "ожидался StoreCleanupBlocked, публикация прошла"

------------------------------------------------------------------------------
-- Публикация
------------------------------------------------------------------------------

publishTests :: TestTree
publishTests =
  testGroup
    "Публикация"
    [ testCase "создаёт .mix и .nsp без служебных файлов, статус managed" $
        withTempStore "publish" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          doesFileExist (mixPath cfg sampleSlug) >>= (@?= True)
          doesFileExist (nspFile cfg sampleSlug) >>= (@?= True)
          rules <- listDirectory (scRulesDir cfg)
          playlists <- listDirectory (scPlaylistsDir cfg)
          assertBool
            ("остались служебные файлы: " <> show (rules ++ playlists))
            (onlyFinalFiles cfg rules && onlyFinalFiles cfg playlists)
          entries <- listOk cfg
          e <- entryFor entries sampleSlug
          peStatus e @?= "managed"
          peManaged e @?= True
          peExternal e @?= False
          peStale e @?= False
          peDraft e @?= False
          peTitle e @?= pdName sampleDto
          peDescription e @?= "Описание"
          pePublic e @?= True
          peLimit e @?= Just 50
          d <- readOk cfg sampleSlug
          pdRaw d @?= False
          assertBool "нет DTO" (isJust (pdDto d))
          assertBool
            ("в предпросмотре нет секции «подборка»: " <> T.unpack (pdMix d))
            ("подборка" `T.isInfixOf` pdMix d)
          assertBool "нет текста .nsp" (isJust (pdNsp d))
    , testCase "повтор без overwrite -> конфликт, с overwrite -> обновление" $
        withTempStore "conflict" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          r1 <- publishPlaylist cfg sampleSlug False sampleDto
          r1 @?= Left (StoreConflict sampleSlug)
          r2 <- publishPlaylist cfg sampleSlug True (sampleDto {pdLimit = Just 7})
          case r2 of
            Left e ->
              assertFailure ("overwrite не удался: " <> T.unpack (storeErrorMessage e))
            Right _ -> pure ()
          d <- readOk cfg sampleSlug
          case pdDto d of
            Just dto -> pdLimit dto @?= Just 7
            Nothing -> assertFailure "после перезаписи нет DTO"
          entries <- listOk cfg
          length entries @?= 1
    , testCase "невалидный DTO: StoreInvalid и ни одного файла" $
        withTempStore "invalid" $ \cfg -> do
          r <- publishPlaylist cfg "nevaldnaya" False (sampleDto {pdName = ""})
          case r of
            Left (StoreInvalid s _) -> s @?= "nevaldnaya"
            Left e -> assertFailure ("другая ошибка: " <> show e)
            Right _ -> assertFailure "ожидался StoreInvalid"
          rules <- listDirectory (scRulesDir cfg)
          playlists <- listDirectory (scPlaylistsDir cfg)
          rules @?= []
          playlists @?= []
    ]

------------------------------------------------------------------------------
-- Slug
------------------------------------------------------------------------------

-- | Slug, которые обязаны быть отклонены при записи.
unsafeSlugs :: [Text]
unsafeSlugs = ["..", "a/b", "a\\b", "UPPER", "-lead", "", ".hidden"]

slugTests :: TestTree
slugTests =
  testGroup
    "Slug"
    [ testCase "slugFromName: транслитерация и fallback" $ do
        slugFromName "Любимые 80-е" @?= "lyubimye-80-e"
        slugFromName "???" @?= "playlist"
        assertBool
          "результат транслитерации не проходит slugSafe"
          (slugSafe (slugFromName "Любимые 80-е"))
    , testCase "небезопасные slug отклоняются и при записи тоже" $
        withTempStore "slug" $ \cfg ->
          forM_ unsafeSlugs $ \s -> do
            assertBool ("slugSafe " <> show s <> " должен быть False") (not (slugSafe s))
            r <- publishPlaylist cfg s False sampleDto
            case r of
              Left StoreUnsafeSlug{} -> pure ()
              other ->
                assertFailure
                  ("slug "
                     <> show s
                     <> ": ожидался StoreUnsafeSlug, получено "
                     <> show other)
    , testCase "чтение: path traversal и отсутствующая подборка" $
        withTempStore "read" $ \cfg -> do
          r1 <- readPlaylist cfg "../evil"
          r1 @?= Left (StoreUnsafeSlug "../evil")
          r2 <- readPlaylist cfg "net-takogo"
          r2 @?= Left (StoreNotFound "net-takogo")
    , testCase "удаление: небезопасный slug и отсутствие файла" $
        withTempStore "del-slug" $ \cfg -> do
          d1 <- deletePlaylistFiles cfg "../../etc"
          d1 @?= Left (StoreUnsafeSlug "../../etc")
          d2 <- deletePlaylistFiles cfg "net-takogo"
          d2 @?= Left (StoreNotFound "net-takogo")
    ]

------------------------------------------------------------------------------
-- Статусы
------------------------------------------------------------------------------

statusTests :: TestTree
statusTests =
  testGroup
    "Статусы managed/external/broken"
    [ testCase "только .mix: флаг draft, статус managed" $
        withTempStore "draft" $ \cfg -> do
          BS.writeFile (mixPath cfg "chernovik") (TE.encodeUtf8 validMix)
          entries <- listOk cfg
          e <- entryFor entries "chernovik"
          peDraft e @?= True
          peStale e @?= False
          peStatus e @?= "managed"
          peManaged e @?= True
    , testCase "только .nsp: флаг stale, статус managed" $
        withTempStore "stale" $ \cfg -> do
          nspBs <- compileNsp sampleDto
          BS.writeFile (nspFile cfg "tolko-nsp") nspBs
          entries <- listOk cfg
          e <- entryFor entries "tolko-nsp"
          peStale e @?= True
          peDraft e @?= False
          peStatus e @?= "managed"
    , testCase ".nsp с неизвестным узлом: external и read-only" $
        withTempStore "external" $ \cfg -> do
          BS.writeFile (nspFile cfg "vnyeshnyaya") externalNsp
          entries <- listOk cfg
          e <- entryFor entries "vnyeshnyaya"
          peStatus e @?= "external"
          peExternal e @?= True
          peManaged e @?= False
          d <- readOk cfg "vnyeshnyaya"
          pdRaw d @?= True
          assertBool "нет DTO у external" (isJust (pdDto d))
    , testCase "мусорный .mix: broken с текстом ошибки" $
        withTempStore "broken" $ \cfg -> do
          BS.writeFile (mixPath cfg "musor") (TE.encodeUtf8 "подборка без кавычек\n")
          entries <- listOk cfg
          e <- entryFor entries "musor"
          peStatus e @?= "broken"
          case peError e of
            Nothing -> assertFailure "нет текста ошибки"
            Just err ->
              assertBool
                ("ошибка не содержит позицию разбора: " <> T.unpack err)
                ("строка 1, столбец" `T.isInfixOf` err)
          d <- readOk cfg "musor"
          pdDto d @?= Nothing
    ]

-- | Валидный @.mix@ без опубликованного @.nsp@.
validMix :: Text
validMix =
  T.unlines
    [ "подборка \"Черновик\""
    , "где все {"
    , "  любимое"
    , "}"
    ]

-- | @.nsp@ с узлом, неизвестным редактору.
externalNsp :: BS.ByteString
externalNsp =
  TE.encodeUtf8
    "{\"name\":\"Внешняя подборка\",\"all\":[{\"unknownop\":{\"title\":\"x\"}}]}"

------------------------------------------------------------------------------
-- Корзина
------------------------------------------------------------------------------

trashTests :: TestTree
trashTests =
  testGroup
    "Корзина"
    [ testCase "удаление -> корзина -> восстановление -> очистка" $
        withTempStore "trash" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          tid <- deleteOk cfg sampleSlug
          assertBool
            ("id без слага: " <> T.unpack tid)
            (("-" <> sampleSlug) `T.isSuffixOf` tid)
          doesFileExist (mixPath cfg sampleSlug) >>= (@?= False)
          doesFileExist (nspFile cfg sampleSlug) >>= (@?= False)
          doesFileExist (scTrashDir cfg </> T.unpack tid </> "meta.txt")
            >>= (@?= True)
          trash <- trashOk cfg
          case [t | t <- trash, teId t == tid] of
            [t] -> do
              teSlug t @?= sampleSlug
              assertBool "в записи нет .mix" (isJust (teMixFile t))
              assertBool "в записи нет .nsp" (isJust (teNspFile t))
            other -> assertFailure ("запись корзины не найдена: " <> show other)
          -- восстановление
          rr <- restoreTrash cfg tid
          rr @?= Right ()
          doesFileExist (mixPath cfg sampleSlug) >>= (@?= True)
          doesFileExist (nspFile cfg sampleSlug) >>= (@?= True)
          trash1 <- trashOk cfg
          trash1 @?= []
          -- повторное удаление и окончательная очистка
          tid2 <- deleteOk cfg sampleSlug
          pr <- purgeTrash cfg tid2
          pr @?= Right ()
          trash2 <- trashOk cfg
          trash2 @?= []
    , testCase "восстановление неизвестной записи -> not_found" $
        withTempStore "restore-miss" $ \cfg -> do
          r <- restoreTrash cfg "20200101T000000-net-takogo"
          r @?= Left (StoreNotFound "20200101T000000-net-takogo")
    , testCase "очистка неизвестной записи -> not_found" $
        withTempStore "purge-miss" $ \cfg -> do
          r <- purgeTrash cfg "20200101T000000-net-takogo"
          r @?= Left (StoreNotFound "20200101T000000-net-takogo")
    ]

------------------------------------------------------------------------------
-- Символические ссылки
------------------------------------------------------------------------------

symlinkTest :: TestTree
symlinkTest = testCase "символическая ссылка блокирует запись" $
  withTempStore "symlink" $ \cfg -> do
    let outside = scRulesDir cfg </> "outside.nsp"
        link = nspFile cfg "linka"
    BS.writeFile outside "{}"
    created <- try (createFileLink outside link) :: IO (Either IOException ())
    case created of
      -- На Windows создание симлинков требует привилегии: без неё
      -- проверку воспроизвести нельзя, тест пропускается.
      Left _ -> pure ()
      Right () -> do
        r <- publishPlaylist cfg "linka" True sampleDto
        case r of
          Left StoreUnsafePath{} -> pure ()
          other ->
            assertFailure
              ("ожидался StoreUnsafePath, получено: " <> show other)

------------------------------------------------------------------------------
-- Переименование опубликованной подборки
------------------------------------------------------------------------------

renameTests :: TestTree
renameTests =
  testGroup
    "Переименование опубликованной подборки"
    [ testCase "обычный rename: новый файл есть, старого нет, state переехал" $
        withTempStore "rename" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          _ <-
            publishFromOk cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          assertFiles
            [ (nspFile cfg sampleSlug, False)
            , (mixPath cfg sampleSlug, False)
            , (nspFile cfg renameNewSlug, True)
            , (mixPath cfg renameNewSlug, True)
            ]
          assertNoLeftovers cfg
          -- двух подборок не появилось
          entries <- listOk cfg
          [peSlug e | e <- entries] @?= [renameNewSlug]
          d <- readOk cfg renameNewSlug
          fmap pdName (pdDto d) @?= Just renameNewName
          rs <- readPublishedState cfg
          case [r | r <- rs, prSlug r == renameNewSlug] of
            [r] -> do
              fmap pfPath (prNsp r) @?= Just (nspFile cfg renameNewSlug)
              fmap pfPath (prMix r) @?= Just (mixPath cfg renameNewSlug)
            other ->
              assertFailure ("нет записи state нового slug: " <> show other)
          [prSlug r | r <- rs, prSlug r /= renameNewSlug] @?= []
    , testCase "несколько последовательных rename: остаётся только последний" $
        withTempStore "rename-seq" $ \cfg -> do
          let s1 = renameNewSlug
              s2 = slugFromName "Третий Вариант"
              s3 = slugFromName "Четвёртый Набор"
          _ <- publishOk cfg sampleSlug sampleDto
          _ <- publishFromOk cfg (Just sampleSlug) s1 False (dtoNamed renameNewName)
          _ <- publishFromOk cfg (Just s1) s2 False (dtoNamed "Третий Вариант")
          _ <- publishFromOk cfg (Just s2) s3 False (dtoNamed "Четвёртый Набор")
          assertFiles
            [ (nspFile cfg sampleSlug, False)
            , (mixPath cfg sampleSlug, False)
            , (nspFile cfg s1, False)
            , (mixPath cfg s1, False)
            , (nspFile cfg s2, False)
            , (mixPath cfg s2, False)
            , (nspFile cfg s3, True)
            , (mixPath cfg s3, True)
            ]
          assertNoLeftovers cfg
          entries <- listOk cfg
          [peSlug e | e <- entries] @?= [s3]
          rs <- readPublishedState cfg
          [prSlug r | r <- rs] @?= [s3]
    , testCase "смена названия без смены filename: перезапись на месте" $
        withTempStore "rename-same" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          let dto2 = sampleDto {pdName = "Любимые ТрекИ!"}
          -- sanity: slug не изменился, rename быть не должно
          slugFromName (pdName dto2) @?= sampleSlug
          _ <- publishFromOk cfg (Just sampleSlug) sampleSlug True dto2
          assertFiles
            [(nspFile cfg sampleSlug, True), (mixPath cfg sampleSlug, True)]
          assertNoLeftovers cfg
          entries <- listOk cfg
          e <- entryFor entries sampleSlug
          peTitle e @?= "Любимые ТрекИ!"
          rs <- readPublishedState cfg
          [prSlug r | r <- rs] @?= [sampleSlug]
    , testCase "целевой файл существует: конфликт, с overwrite - замена" $
        withTempStore "rename-clash" $ \cfg -> do
          let other = dtoNamed renameNewName
          _ <- publishOk cfg sampleSlug sampleDto
          _ <- publishOk cfg renameNewSlug other
          -- без overwrite: конфликт до записи, файлы не тронуты
          r <-
            publishPlaylistFrom cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          r @?= Left (StoreConflict renameNewSlug)
          assertFiles
            [ (nspFile cfg sampleSlug, True)
            , (mixPath cfg sampleSlug, True)
            , (nspFile cfg renameNewSlug, True)
            , (mixPath cfg renameNewSlug, True)
            ]
          rs0 <- readPublishedState cfg
          length rs0 @?= 2
          -- с overwrite: целевая пара заменяется, старая удаляется
          _ <-
            publishFromOk cfg (Just sampleSlug) renameNewSlug True (dtoNamed renameNewName)
          assertFiles
            [ (nspFile cfg sampleSlug, False)
            , (mixPath cfg sampleSlug, False)
            , (nspFile cfg renameNewSlug, True)
            , (mixPath cfg renameNewSlug, True)
            ]
          assertNoLeftovers cfg
          rs <- readPublishedState cfg
          [prSlug x | x <- rs] @?= [renameNewSlug]
    , testCase "старый файл исчез внешне: публикация проходит (OldGone)" $
        withTempStore "rename-gone" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          -- .nsp удалён извне; .mix ещё существует и подтверждён state
          removeFile (nspFile cfg sampleSlug)
          _ <-
            publishFromOk cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          assertFiles
            [ (nspFile cfg sampleSlug, False)
            , (mixPath cfg sampleSlug, False)
            , (nspFile cfg renameNewSlug, True)
            , (mixPath cfg renameNewSlug, True)
            ]
          assertNoLeftovers cfg
          rs <- readPublishedState cfg
          [prSlug r | r <- rs] @?= [renameNewSlug]
    , testCase "содержимое старого изменено извне: blocked, ничего не тронуто" $
        withTempStore "rename-foreign" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          BS.writeFile (nspFile cfg sampleSlug) externalNsp
          expectBlocked (nspFile cfg sampleSlug) $
            publishPlaylistFrom cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          assertFiles
            [ (nspFile cfg sampleSlug, True)
            , (mixPath cfg sampleSlug, True)
            , (nspFile cfg renameNewSlug, False)
            , (mixPath cfg renameNewSlug, False)
            ]
          -- старый файл остался в том виде, в каком его оставили извне
          BS.readFile (nspFile cfg sampleSlug) >>= (@?= externalNsp)
          rs <- readPublishedState cfg
          [prSlug r | r <- rs] @?= [sampleSlug]
          assertNoLeftovers cfg
    , testCase "файлы записаны мимо приложения: blocked, файлы целы" $
        withTempStore "rename-nostate" $ \cfg -> do
          -- пара .mix + .nsp без записи в persisted state
          let prev = "vneshniy"
          BS.writeFile (mixPath cfg prev) (TE.encodeUtf8 validMix)
          nspBs <- compileNsp (dtoNamed renameNewName)
          BS.writeFile (nspFile cfg prev) nspBs
          expectBlocked (nspFile cfg prev) $
            publishPlaylistFrom cfg (Just prev) renameNewSlug False (dtoNamed renameNewName)
          assertFiles
            [ (mixPath cfg prev, True)
            , (nspFile cfg prev, True)
            , (nspFile cfg renameNewSlug, False)
            , (mixPath cfg renameNewSlug, False)
            ]
          BS.readFile (nspFile cfg prev) >>= (@?= nspBs)
          -- state не появился: публикация отменена до записи
          doesFileExist (stateFilePath cfg) >>= (@?= False)
          assertNoLeftovers cfg
    , testCase "ошибка записи нового: старые файлы остаются рабочими" $
        withTempStore "rename-writefail" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          -- staging-каталог на месте временного файла нового .mix
          let stage =
                scRulesDir cfg </> ("." ++ T.unpack renameNewSlug ++ ".mix.tmp")
          createDirectory stage
          r <-
            publishPlaylistFrom cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          case r of
            Left StoreIo{} -> pure ()
            other ->
              assertFailure ("ожидался StoreIo, получено: " <> show other)
          removeDirectory stage
          assertFiles
            [ (nspFile cfg sampleSlug, True)
            , (mixPath cfg sampleSlug, True)
            , (nspFile cfg renameNewSlug, False)
            , (mixPath cfg renameNewSlug, False)
            ]
          rs <- readPublishedState cfg
          [prSlug x | x <- rs] @?= [sampleSlug]
          -- старая подборка по-прежнему читается
          d <- readOk cfg sampleSlug
          fmap pdName (pdDto d) @?= Just (pdName sampleDto)
          assertNoLeftovers cfg
    , testCase "сбой удаления старого: cleanup_failed, не скрыт как успех" $
        withTempStore "rename-cleanupfail" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          let boom :: FilePath -> IO ()
              boom _ = throwIO (userError "сбой удаления (тест)")
          r <-
            publishPlaylistWith boom cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          case r of
            Left (StoreCleanupFailed p _) -> p @?= nspFile cfg sampleSlug
            other ->
              assertFailure ("ожидался StoreCleanupFailed, получено: " <> show other)
          -- новый файл записан, старый остался, state не обновлён
          assertFiles
            [ (nspFile cfg renameNewSlug, True)
            , (mixPath cfg renameNewSlug, True)
            , (nspFile cfg sampleSlug, True)
            , (mixPath cfg sampleSlug, True)
            ]
          rs <- readPublishedState cfg
          [prSlug x | x <- rs] @?= [sampleSlug]
          assertNoLeftovers cfg
    , testCase "чужие .nsp/.mix в каталогах не затронуты" $
        withTempStore "rename-others" $ \cfg -> do
          let other = sampleDto {pdName = "Другая Подборка"}
              otherSlug = slugFromName (pdName other)
          _ <- publishOk cfg sampleSlug sampleDto
          _ <- publishOk cfg otherSlug other
          otherNsp0 <- BS.readFile (nspFile cfg otherSlug)
          otherMix0 <- BS.readFile (mixPath cfg otherSlug)
          _ <-
            publishFromOk cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          assertFiles
            [(nspFile cfg otherSlug, True), (mixPath cfg otherSlug, True)]
          BS.readFile (nspFile cfg otherSlug) >>= (@?= otherNsp0)
          BS.readFile (mixPath cfg otherSlug) >>= (@?= otherMix0)
          assertNoLeftovers cfg
          rs <- readPublishedState cfg
          sort [prSlug r | r <- rs] @?= sort [otherSlug, renameNewSlug]
    , testCase "рестарт: state с диска указывает на новый путь, identity живёт" $
        withTempStore "rename-restart" $ \cfg -> do
          _ <- publishOk cfg sampleSlug sampleDto
          _ <-
            publishFromOk cfg (Just sampleSlug) renameNewSlug False (dtoNamed renameNewName)
          -- «рестарт»: читается только persisted state с диска
          rs <- readPublishedState cfg
          case rs of
            [r] -> do
              prSlug r @?= renameNewSlug
              fmap pfPath (prNsp r) @?= Just (nspFile cfg renameNewSlug)
            other -> assertFailure ("неожиданный state: " <> show other)
          -- следующий rename использует сохранённую identity
          let s2 = slugFromName "Третий Вариант"
          _ <- publishFromOk cfg (Just renameNewSlug) s2 False (dtoNamed "Третий Вариант")
          assertFiles
            [ (nspFile cfg sampleSlug, False)
            , (mixPath cfg sampleSlug, False)
            , (nspFile cfg renameNewSlug, False)
            , (mixPath cfg renameNewSlug, False)
            , (nspFile cfg s2, True)
            , (mixPath cfg s2, True)
            ]
          assertNoLeftovers cfg
          rs2 <- readPublishedState cfg
          [prSlug r | r <- rs2] @?= [s2]
    ]

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

storeTests :: TestTree
storeTests =
  testGroup
    "Store"
    [ publishTests
    , slugTests
    , statusTests
    , trashTests
    , symlinkTest
    , renameTests
    ]
