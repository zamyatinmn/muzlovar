{-# LANGUAGE OverloadedStrings #-}

-- | Интеграционные тесты HTTP-слоя Muzlovar: Basic Auth, публичные
-- маршруты, валидация, жизненный цикл подборки, корзина,
-- external-readonly и структурированные ошибки API.
module ServerTests (serverTests) where

import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nspeller.Muzlovar.Server
import Nspeller.Muzlovar.Store
import Nspeller.Muzlovar.Types
import StoreTests (withTempStore)
import System.Directory (doesFileExist, listDirectory, createDirectory, removeFile)
import System.FilePath ((</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit
import Network.HTTP.Types
  ( Header
  , hContentType
  , hWWWAuthenticate
  , methodDelete
  , methodGet
  , methodPost
  , methodPut
  , parseQuery
  , statusCode
  )
import Network.Wai (Application, Request (..), defaultRequest)
import Network.Wai.Test (SRequest (..), SResponse (..), runSession, srequest)

------------------------------------------------------------------------------
-- Построение запросов
------------------------------------------------------------------------------

-- | Учётные данные тестового сервера.
authHeader :: Header
authHeader = ("Authorization", "Basic YWRtaW46c2VjcmV0") -- base64("admin:secret")

-- | Запрос: метод, путь, строка запроса, тело, дополнительные заголовки.
sreq ::
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  LBS.ByteString ->
  [Header] ->
  SRequest
sreq method path query body hdrs =
  SRequest
    defaultRequest
      { requestMethod = method
      , rawPathInfo = path
      , pathInfo = splitPath path
      , queryString = parseQuery query
      , requestHeaders = (hContentType, "application/json") : hdrs
      }
    body

-- | Путь WAI -> компоненты маршрутизации.
splitPath :: BS.ByteString -> [Text]
splitPath p = [TE.decodeUtf8 c | c <- BS.split 47 p, not (BS.null c)]

run1 :: Application -> SRequest -> IO SResponse
run1 app r = runSession (srequest r) app

statusOf :: SResponse -> Int
statusOf = statusCode . simpleStatus

------------------------------------------------------------------------------
-- Разбор ответов
------------------------------------------------------------------------------

-- | Значение поля верхнего уровня JSON-тела.
bodyField :: Key -> SResponse -> Maybe Value
bodyField k r = case (eitherDecode (simpleBody r) :: Either String Value) of
  Right (Object o) -> KM.lookup k o
  _ -> Nothing

-- | Коды из массива @errors@ (или пусто).
errorCodes :: SResponse -> [Text]
errorCodes r = case bodyField "errors" r of
  Just (Array v) -> [codeOf e | Object e <- foldr (:) [] v]
  _ -> []
  where
    codeOf :: KM.KeyMap Value -> Text
    codeOf o = case KM.lookup "code" o of
      Just (String c) -> c
      _ -> ""

-- | Идентификаторы записей корзины в ответе.
trashIds :: SResponse -> [Text]
trashIds r = case bodyField "trash" r of
  Just (Array v) ->
    [t | Object e <- foldr (:) [] v, Just (String t) <- [KM.lookup "id" e]]
  _ -> []

-- | @trashId@ из ответа на удаление (пусто, если поля нет).
trashIdOf :: SResponse -> Text
trashIdOf r = case bodyField "trashId" r of
  Just (String t) -> t
  _ -> ""

-- | Есть ли фрагмент в тексте ответа.
bodyContains :: LBS.ByteString -> SResponse -> Bool
bodyContains needle r = LBS.toStrict needle `BS.isInfixOf` LBS.toStrict (simpleBody r)

assertHasCode :: Text -> SResponse -> Assertion
assertHasCode code r =
  assertBool
    ("нет кода «" <> T.unpack code <> "»; тело: " <> show (simpleBody r))
    (code `elem` errorCodes r)

------------------------------------------------------------------------------
-- Тестовые DTO
------------------------------------------------------------------------------

validDto :: PlaylistDto
validDto =
  PlaylistDto
    { pdName = "Тестовая подборка"
    , pdDescription = Just "Для проверки API"
    , pdPublic = False
    , pdRoot = GroupDto "all" [ItemCond (CondDto "любимое" "bare" Nothing)]
    , pdSort = Just SortRandomDto
    , pdLimit = Just 10
    }

unknownFieldDto :: PlaylistDto
unknownFieldDto =
  validDto
    { pdRoot = GroupDto "all" [ItemCond (CondDto "меме" "gt" (Just (Number 1)))]
    }

-- | @.nsp@ с узлом, неизвестным редактору (пишется напрямую).
externalNsp :: BS.ByteString
externalNsp =
  TE.encodeUtf8
    "{\"name\":\"Внешняя подборка\",\"all\":[{\"unknownop\":{\"title\":\"x\"}}]}"

------------------------------------------------------------------------------
-- Сервер во временном хранилище
------------------------------------------------------------------------------

-- | Приложение поверх временного хранилища: admin/secret, Subsonic
-- не настроен.
withServer :: String -> (Application -> StoreConfig -> IO a) -> IO a
withServer label act =
  withTempStore label $ \cfg -> do
    app <- muzlovarApp (ServerConfig cfg Nothing (Just ("admin", "secret")))
    act app cfg

-- | Приложение без авторизации (scAuth = Nothing).
withOpenServer :: String -> (Application -> StoreConfig -> IO a) -> IO a
withOpenServer label act =
  withTempStore label $ \cfg -> do
    app <- muzlovarApp (ServerConfig cfg Nothing Nothing)
    act app cfg

------------------------------------------------------------------------------
-- Авторизация
------------------------------------------------------------------------------

authTests :: TestTree
authTests =
  testGroup
    "авторизация"
    [ testCase "авторизация включена: /health, /static и favicon открыты, API закрыт" $
        withServer "auth" $ \app _cfg -> do
          h <- run1 app (sreq methodGet "/health" "" "" [])
          statusOf h @?= 200
          assertBool ("нет статуса ok: " <> show (simpleBody h)) (bodyContains "\"ok\"" h)
          c <- run1 app (sreq methodGet "/static/muzlovar.js" "" "" [])
          statusOf c @?= 200
          -- фавикон браузер запрашивает без учётных данных
          fi <- run1 app (sreq methodGet "/favicon.ico" "" "" [])
          statusOf fi @?= 200
          assertBool "favicon не image/x-icon" $
            lookup hContentType (simpleHeaders fi) == Just "image/x-icon"
          -- без учётных данных
          u <- run1 app (sreq methodGet "/api/schema" "" "" [])
          statusOf u @?= 401
          assertBool
            "нет WWW-Authenticate"
            (isJust (lookup hWWWAuthenticate (simpleHeaders u)))
          -- некорректная base64
          w1 <- run1 app (sreq methodGet "/api/schema" "" "" [("Authorization", "Basic !!!")])
          statusOf w1 @?= 401
          -- неверный пароль: base64("admin:") = YWRtaW46
          w2 <- run1 app (sreq methodGet "/api/schema" "" "" [("Authorization", "Basic YWRtaW46")])
          statusOf w2 @?= 401
          -- верные учётные данные
          s <- run1 app (sreq methodGet "/api/schema" "" "" [authHeader])
          statusOf s @?= 200
          bodyField "version" s @?= Just (Number 1)

    , testCase "авторизация выключена: API открыт без учётных данных" $
        withOpenServer "noauth" $ \app _cfg -> do
          u <- run1 app (sreq methodGet "/api/schema" "" "" [])
          statusOf u @?= 200
          bodyField "version" u @?= Just (Number 1)
          p <- run1 app (sreq methodGet "/" "" "" [])
          statusOf p @?= 200
          h <- run1 app (sreq methodGet "/health" "" "" [])
          statusOf h @?= 200
    , testCase "шапка и head: логотип и фавикон подключены" $
        withOpenServer "brand" $ \app _cfg -> do
          p <- run1 app (sreq methodGet "/" "" "" [])
          statusOf p @?= 200
          assertBool "нет ссылки на фавикон в head" $
            bodyContains "rel=\"icon\"" p
          assertBool "нет href фавикона" $
            bodyContains "href=\"/static/favicon.ico\"" p
          assertBool "нет логотипа в шапке" $
            bodyContains "src=\"/static/logo.png\"" p
          assertBool "нет класса логотипа" $
            bodyContains "class=\"logo-img\"" p
          -- сами ресурсы отдаются с корректным content-type
          l <- run1 app (sreq methodGet "/static/logo.png" "" "" [])
          statusOf l @?= 200
          assertBool "logo не image/png" $
            lookup hContentType (simpleHeaders l) == Just "image/png"
          f <- run1 app (sreq methodGet "/static/favicon.ico" "" "" [])
          statusOf f @?= 200
          assertBool "favicon не image/x-icon" $
            lookup hContentType (simpleHeaders f) == Just "image/x-icon"
          -- PNG-заголовок, а не пустая заглушка
          assertBool "logo не начинается с PNG-сигнатуры" $
            BS.isPrefixOf (BS.pack [0x89, 0x50, 0x4E, 0x47]) (LBS.toStrict (simpleBody l))
    ]

------------------------------------------------------------------------------
-- Валидация
------------------------------------------------------------------------------

validateTests :: TestTree
validateTests =
  testCase "/api/validate: проверка без записи на диск" $
    withServer "validate" $ \app cfg -> do
      ok <- run1 app (sreq methodPost "/api/validate" "" (encode validDto) [authHeader])
      statusOf ok @?= 200
      bodyField "ok" ok @?= Just (Bool True)
      case bodyField "mix" ok of
        Just (String m) ->
          assertBool ("в предпросмотре нет «подборка»: " <> T.unpack m) ("подборка" `T.isInfixOf` m)
        other -> assertFailure ("нет .mix в ответе: " <> show other)
      case bodyField "nsp" ok of
        Just (String n) ->
          assertBool
            ("в предпросмотре .nsp нет массива all: " <> T.unpack n)
            ("\"all\"" `T.isInfixOf` n)
        other -> assertFailure ("нет .nsp в ответе: " <> show other)
      -- итоговый slug (будущий filename) — блок «Опубликовано /
      -- Будет опубликовано» сравнивает его с опубликованным путём
      bodyField "slug" ok @?= Just (String (slugFromName (pdName validDto)))
      -- предупреждения есть всегда (пустой массив, если их нет)
      case bodyField "warnings" ok of
        Just (Array _) -> pure ()
        other -> assertFailure ("нет ключа warnings в ответе: " <> show other)
      -- структурная ошибка
      bad <- run1 app (sreq methodPost "/api/validate" "" (encode validDto {pdName = ""}) [authHeader])
      statusOf bad @?= 422
      assertHasCode "invalid_tree" bad
      assertBool "список ошибок пуст" (not (null (errorCodes bad)))
      -- ошибка валидации ядра
      sem <- run1 app (sreq methodPost "/api/validate" "" (encode unknownFieldDto) [authHeader])
      statusOf sem @?= 422
      assertHasCode "validation" sem
      -- ничего не записано на диск
      rules <- listDirectory (scRulesDir cfg)
      rules @?= []

------------------------------------------------------------------------------
-- Жизненный цикл подборки
------------------------------------------------------------------------------

lifecycleTests :: TestTree
lifecycleTests =
  testCase "жизненный цикл: POST -> GET -> DELETE -> корзина" $
    withServer "life" $ \app cfg -> do
      let slug = slugFromName (pdName validDto)
          detailPath = "/api/playlists/" <> TE.encodeUtf8 slug
      -- создание
      c1 <- run1 app (sreq methodPost "/api/playlists" "" (encode validDto) [authHeader])
      statusOf c1 @?= 201
      -- повтор без перезаписи -> конфликт
      c2 <- run1 app (sreq methodPost "/api/playlists" "" (encode validDto) [authHeader])
      statusOf c2 @?= 409
      assertHasCode "conflict" c2
      -- перезапись по подтверждению
      c3 <- run1 app (sreq methodPost "/api/playlists" "overwrite=1" (encode validDto) [authHeader])
      statusOf c3 @?= 201
      -- список
      l <- run1 app (sreq methodGet "/api/playlists" "" "" [authHeader])
      statusOf l @?= 200
      case bodyField "playlists" l of
        Just (Array v) -> length (foldr (:) [] v) @?= 1
        other -> assertFailure ("нет playlists: " <> show other)
      -- детальная карточка
      d <- run1 app (sreq methodGet detailPath "" "" [authHeader])
      statusOf d @?= 200
      bodyField "external" d @?= Just (Bool False)
      bodyField "editable" d @?= Just (Bool True)
      -- неизвестный slug
      nf <- run1 app (sreq methodGet "/api/playlists/net-takogo" "" "" [authHeader])
      statusOf nf @?= 404
      assertHasCode "not_found" nf
      -- удаление в корзину
      del <- run1 app (sreq methodDelete detailPath "" "" [authHeader])
      statusOf del @?= 200
      let tid = trashIdOf del
      assertBool "нет trashId в ответе" (not (T.null tid))
      case bodyField "subsonic" del of
        Just (Object sub) -> KM.lookup "attempted" sub @?= Just (Bool False)
        other -> assertFailure ("нет статуса subsonic: " <> show other)
      doesFileExist (scRulesDir cfg </> (T.unpack slug ++ ".mix")) >>= (@?= False)
      -- корзина содержит запись
      tl <- run1 app (sreq methodGet "/api/trash" "" "" [authHeader])
      statusOf tl @?= 200
      assertBool
        ("нет записи " <> T.unpack tid <> " в корзине: " <> show (trashIds tl))
        (tid `elem` trashIds tl)
      -- восстановление
      rs <-
        run1
          app
          (sreq
             methodPost
             ("/api/trash/" <> TE.encodeUtf8 tid <> "/restore")
             ""
             "{}"
             [authHeader])
      statusOf rs @?= 200
      bodyField "restored" rs @?= Just (Bool True)
      doesFileExist (scRulesDir cfg </> (T.unpack slug ++ ".mix")) >>= (@?= True)
      doesFileExist (scPlaylistsDir cfg </> (T.unpack slug ++ ".nsp")) >>= (@?= True)
      -- повторное удаление и очистка корзины
      del2 <- run1 app (sreq methodDelete detailPath "" "" [authHeader])
      statusOf del2 @?= 200
      let tid2 = trashIdOf del2
      assertBool "нет второго trashId" (not (T.null tid2))
      pg <-
        run1 app (sreq methodDelete ("/api/trash/" <> TE.encodeUtf8 tid2) "" "" [authHeader])
      statusOf pg @?= 200
      bodyField "purged" pg @?= Just (Bool True)
      tl2 <- run1 app (sreq methodGet "/api/trash" "" "" [authHeader])
      trashIds tl2 @?= []

------------------------------------------------------------------------------
-- Переименование опубликованной подборки (PUT)
------------------------------------------------------------------------------

-- | Slug'ы из массива @playlists@ ответа списка.
entrySlugs :: SResponse -> [Text]
entrySlugs l = case bodyField "playlists" l of
  Just (Array v) ->
    [s | Object e <- foldr (:) [] v, Just (String s) <- [KM.lookup "slug" e]]
  _ -> []

-- | Итоговый slug: так же его вычисляет PUT /api/playlists/:slug.
renameSlug :: Text
renameSlug = slugFromName (pdName renameDto)

renameDto :: PlaylistDto
renameDto = validDto {pdName = "Новое Название"}

-- | @.nsp@, скомпилированный из DTO (для пар, записанных мимо
-- приложения).
compiledNsp :: PlaylistDto -> BS.ByteString
compiledNsp dto = case compilePlaylistDto dto of
  Right c -> LBS.toStrict (cmpNsp c)
  Left es -> error ("compilePlaylistDto: " <> show es)

renameTests :: TestTree
renameTests =
  testGroup
    "PUT: переименование опубликованной подборки"
    [ testCase "новый файл опубликован, старый удалён, список и страница обновлены" $
        withServer "put-rename" $ \app cfg -> do
          let slug0 = slugFromName (pdName validDto)
              oldNsp = scPlaylistsDir cfg </> (T.unpack slug0 ++ ".nsp")
              oldMix = scRulesDir cfg </> (T.unpack slug0 ++ ".mix")
              newNsp = scPlaylistsDir cfg </> (T.unpack renameSlug ++ ".nsp")
              newMix = scRulesDir cfg </> (T.unpack renameSlug ++ ".mix")
          c <-
            run1 app (sreq methodPost "/api/playlists" "" (encode validDto) [authHeader])
          statusOf c @?= 201
          pu <-
            run1
              app
              ( sreq
                  methodPut
                  ("/api/playlists/" <> TE.encodeUtf8 slug0)
                  ""
                  (encode renameDto)
                  [authHeader]
              )
          statusOf pu @?= 200
          case bodyField "entry" pu of
            Just (Object o) -> KM.lookup "slug" o @?= Just (String renameSlug)
            other -> assertFailure ("нет entry в ответе PUT: " <> show other)
          -- новый файл существует, старого нет
          doesFileExist newNsp >>= (@?= True)
          doesFileExist newMix >>= (@?= True)
          doesFileExist oldNsp >>= (@?= False)
          doesFileExist oldMix >>= (@?= False)
          -- список без старого slug
          l <- run1 app (sreq methodGet "/api/playlists" "" "" [authHeader])
          entrySlugs l @?= [renameSlug]
          -- старый slug больше не найден
          g0 <-
            run1 app (sreq methodGet ("/api/playlists/" <> TE.encodeUtf8 slug0) "" "" [authHeader])
          statusOf g0 @?= 404
          assertHasCode "not_found" g0
          -- persisted state переехал на новый путь
          rs <- readPublishedState cfg
          [prSlug r | r <- rs] @?= [renameSlug]
          fmap (fmap pfPath . prNsp) rs @?= [Just newNsp]
          -- страница нового slug показывает новый путь
          pg <-
            run1 app (sreq methodGet ("/edit/" <> TE.encodeUtf8 renameSlug) "" "" [authHeader])
          statusOf pg @?= 200
          assertBool
            "нет data-published=\"1\""
            (bodyContains "data-published=\"1\"" pg)
          assertBool
            "нет data-published-path нового файла"
            ( bodyContains
                (LBS.fromStrict(TE.encodeUtf8 ("data-published-path=\"" <> T.pack newNsp <> "\"")))
                pg
            )
          assertBool
            "нет data-slug нового slug"
            ( bodyContains
                (LBS.fromStrict(TE.encodeUtf8 ("data-slug=\"" <> renameSlug <> "\"")))
                pg
            )
    , testCase "cleanup_blocked (422): файлы без state не удаляются" $
        withServer "put-rename-blocked" $ \app cfg -> do
          -- пара записана мимо приложения: записи в state нет
          let prev = "vneshniy"
              oldNsp = scPlaylistsDir cfg </> (prev ++ ".nsp")
              oldMix = scRulesDir cfg </> (prev ++ ".mix")
              blockedDto = validDto {pdName = "Blocked E2E"}
              blockedSlug = slugFromName (pdName blockedDto)
          BS.writeFile
            oldMix
            (TE.encodeUtf8 "подборка \"Черновик\"\nгде все {\n  любимое\n}\n")
          BS.writeFile oldNsp (compiledNsp validDto)
          oldNsp0 <- BS.readFile oldNsp
          oldMix0 <- BS.readFile oldMix
          pu <-
            run1
              app
              ( sreq
                  methodPut
                  ("/api/playlists/" <> TE.encodeUtf8 (T.pack prev))
                  ""
                  (encode blockedDto)
                  [authHeader]
              )
          statusOf pu @?= 422
          assertHasCode "cleanup_blocked" pu
          -- файлы не тронуты, новый target не создан, state не появился
          BS.readFile oldNsp >>= (@?= oldNsp0)
          BS.readFile oldMix >>= (@?= oldMix0)
          doesFileExist (scPlaylistsDir cfg </> (T.unpack blockedSlug ++ ".nsp"))
            >>= (@?= False)
          doesFileExist (scRulesDir cfg </> (T.unpack blockedSlug ++ ".mix"))
            >>= (@?= False)
          doesFileExist (stateFilePath cfg) >>= (@?= False)
    ]

------------------------------------------------------------------------------
-- External: только чтение
------------------------------------------------------------------------------

externalTests :: TestTree
externalTests =
  testCase "внешняя подборка доступна только для чтения" $
    withServer "ext" $ \app cfg -> do
      let slug = "vnyeshnyaya" :: Text
          detailPath = "/api/playlists/" <> TE.encodeUtf8 slug
      BS.writeFile (scPlaylistsDir cfg </> (T.unpack slug ++ ".nsp")) externalNsp
      g <- run1 app (sreq methodGet detailPath "" "" [authHeader])
      statusOf g @?= 200
      bodyField "external" g @?= Just (Bool True)
      bodyField "editable" g @?= Just (Bool False)
      -- PUT запрещён до разбора тела
      pu <- run1 app (sreq methodPut detailPath "" (encode validDto) [authHeader])
      statusOf pu @?= 409
      assertHasCode "external_readonly" pu
      -- файл не изменился
      nspText <- BS.readFile (scPlaylistsDir cfg </> (T.unpack slug ++ ".nsp"))
      nspText @?= externalNsp
      -- в списке статус external
      l <- run1 app (sreq methodGet "/api/playlists" "" "" [authHeader])
      case bodyField "playlists" l of
        Just (Array v) -> case foldr (:) [] v of
          [Object e] -> KM.lookup "status" e @?= Just (String "external")
          other -> assertFailure ("неожиданный список: " <> show other)
        other -> assertFailure ("нет playlists: " <> show other)

------------------------------------------------------------------------------
-- Ошибки API
------------------------------------------------------------------------------

apiErrorTests :: TestTree
apiErrorTests =
  testCase "структурированные ошибки API" $
    withServer "errs" $ \app _cfg -> do
      -- битый JSON тела
      bj <- run1 app (sreq methodPost "/api/playlists" "" "{не json" [authHeader])
      statusOf bj @?= 422
      assertHasCode "bad_json" bj
      -- неизвестный /api/* маршрут
      nr <- run1 app (sreq methodGet "/api/neto-such" "" "" [authHeader])
      statusOf nr @?= 404
      assertHasCode "not_found" nr
      -- неизвестная обычная страница — HTML с 404
      np <- run1 app (sreq methodGet "/neto" "" "" [authHeader])
      statusOf np @?= 404
      -- очистка неизвестной записи корзины
      pt <- run1 app (sreq methodDelete "/api/trash/20200101T000000-net" "" "" [authHeader])
      statusOf pt @?= 404
      assertHasCode "not_found" pt

------------------------------------------------------------------------------
-- Юниты авторизации и перевода ошибок
------------------------------------------------------------------------------

statusCase :: TestName -> StoreError -> Int -> TestTree
statusCase name e expected = testCase name $
  statusCode (storeErrorStatus e) @?= expected

codeCase :: TestName -> StoreError -> Text -> TestTree
codeCase name e expected = testCase name $
  case fst (storeErrorToApi e) of
    err : _ -> aeCode err @?= expected
    [] -> assertFailure "пустой список ошибок"

unitTests :: TestTree
unitTests =
  testGroup
    "Юниты"
    [ testGroup
        "constantTimeEq"
        [ testCase "равные строки" $ constantTimeEq "abc" "abc" @?= True
        , testCase "разные строки одной длины" $ constantTimeEq "abc" "abd" @?= False
        , testCase "разной длины" $ constantTimeEq "abc" "abcd" @?= False
        , testCase "обе пустые" $ constantTimeEq "" "" @?= True
        , testCase "одна пустая" $ constantTimeEq "" "a" @?= False
        ]
    , testGroup
        "storeErrorStatus/storeErrorToApi"
        [ statusCase "not_found -> 404" (StoreNotFound "x") 404
        , statusCase "unsafe_slug -> 404" (StoreUnsafeSlug "x") 404
        , statusCase "unsafe_path -> 422" (StoreUnsafePath "x") 422
        , statusCase "conflict -> 409" (StoreConflict "x") 409
        , statusCase
            "validation_failed -> 422"
            (StoreInvalid "x" [apiError "validation" "ошибка"])
            422
        , statusCase "io_error -> 500" (StoreIo "x") 500
        , statusCase "partial_delete -> 500" (StorePartial "x") 500
        , statusCase "cleanup_blocked -> 422" (StoreCleanupBlocked "x.nsp" "почему") 422
        , statusCase "cleanup_failed -> 500" (StoreCleanupFailed "x.nsp" "почему") 500
        , codeCase "код not_found" (StoreNotFound "x") "not_found"
        , codeCase "код unsafe_slug" (StoreUnsafeSlug "x") "unsafe_slug"
        , codeCase "код unsafe_path" (StoreUnsafePath "x") "unsafe_path"
        , codeCase "код conflict" (StoreConflict "x") "conflict"
        , codeCase "код io_error" (StoreIo "x") "io_error"
        , codeCase "код partial_delete" (StorePartial "x") "partial_delete"
        , codeCase "код cleanup_blocked" (StoreCleanupBlocked "x.nsp" "почему") "cleanup_blocked"
        , codeCase "код cleanup_failed" (StoreCleanupFailed "x.nsp" "почему") "cleanup_failed"
        ]
    ]

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

serverTests :: TestTree
serverTests =
  testGroup
    "Server"
    [ authTests
    , validateTests
    , lifecycleTests
    , renameTests
    , externalTests
    , apiErrorTests
    , unitTests
    , artworkTests
    ]

artworkTests :: TestTree
artworkTests = testGroup "Artwork"
  ([ testCase ("upload " ++ ext ++ ": original bytes and MIME") $
       withOpenServer ("art-" ++ ext) $ \app cfg -> do
         (_, endpoint) <- createArtworkPlaylist app
         bytes <- BS.readFile ("test/artwork/sample." ++ ext)
         -- Deliberately claim JSON: binary detection determines the format.
         uploaded <- run1 app (sreq methodPut endpoint "" (LBS.fromStrict bytes) [])
         statusOf uploaded @?= 200
         downloaded <- run1 app (sreq methodGet endpoint "" "" [])
         statusOf downloaded @?= 200
         lookup hContentType (simpleHeaders downloaded) @?= Just mime
         simpleBody downloaded @?= LBS.fromStrict bytes
         BS.readFile (scPlaylistsDir cfg </> T.unpack (slugFromName (pdName validDto)) ++ "." ++ ext) >>= (@?= bytes)
     | (ext, mime) <- [("png", "image/png"), ("jpg", "image/jpeg"), ("webp", "image/webp"), ("gif", "image/gif")]]
   ++ [ testCase "reject unsupported file and MIME spoofing" $
        withOpenServer "art-invalid" $ \app _ -> do
          (_, endpoint) <- createArtworkPlaylist app
          invalid <- run1 app (sreq methodPut endpoint "" "<svg>not a supported image</svg>" [(hContentType, "image/png")])
          statusOf invalid @?= 415
          assertHasCode "unsupported_artwork" invalid
      , testCase "truncated supported images are rejected" $
        withOpenServer "art-truncated" $ \app _ -> do
          (_, endpoint) <- createArtworkPlaylist app
          mapM_ (\ext -> do
            bytes <- BS.readFile ("test/artwork/sample." ++ ext)
            r <- run1 app (sreq methodPut endpoint "" (LBS.fromStrict (BS.take (BS.length bytes `div` 2) bytes)) [])
            statusOf r @?= 415) ["png", "jpg", "webp", "gif"]
      , testCase "reject >10 MB, existing artwork retained" $
        withOpenServer "art-large" $ \app _ -> do
          (_, endpoint) <- createArtworkPlaylist app
          png <- BS.readFile "test/artwork/sample.png"
          run1 app (sreq methodPut endpoint "" (LBS.fromStrict png) []) >>= (\r -> statusOf r @?= 200)
          large <- run1 app (sreq methodPut endpoint "" (LBS.fromStrict (png <> BS.replicate artworkLimit 0)) [])
          statusOf large @?= 413
          assertHasCode "artwork_too_large" large
          readBack <- run1 app (sreq methodGet endpoint "" "" [])
          simpleBody readBack @?= LBS.fromStrict png
      , testCase "manual .jpeg and uppercase sidecar detection" $
        withOpenServer "art-manual" $ \app cfg -> do
          (slug, endpoint) <- createArtworkPlaylist app
          jpeg <- BS.readFile "test/artwork/sample.jpg"
          BS.writeFile (scPlaylistsDir cfg </> T.unpack slug ++ ".JPEG") jpeg
          getArt <- run1 app (sreq methodGet endpoint "" "" [])
          statusOf getArt @?= 200
          lookup hContentType (simpleHeaders getArt) @?= Just "image/jpeg"
          simpleBody getArt @?= LBS.fromStrict jpeg
          detail <- run1 app (sreq methodGet ("/api/playlists/" <> TE.encodeUtf8 slug) "" "" [])
          case bodyField "entry" detail of
            Just (Object e) -> KM.lookup "artwork" e @?= Just (Bool True)
            _ -> assertFailure "Missing entry"
          page <- run1 app (sreq methodGet "/" "" "" [])
          assertBool "list thumbnail missing" (bodyContains "list-artwork" page)
      , testCase "replacement jpg -> png removes every old extension" $
        withOpenServer "art-replace" $ \app cfg -> do
          (slug, endpoint) <- createArtworkPlaylist app
          jpeg <- BS.readFile "test/artwork/sample.jpg"
          png <- BS.readFile "test/artwork/sample.png"
          let old ext = scPlaylistsDir cfg </> T.unpack slug ++ ext
          mapM_ (\ext -> BS.writeFile (old ext) jpeg) [".jpg", ".jpeg", ".GIF"]
          r <- run1 app (sreq methodPut endpoint "" (LBS.fromStrict png) [])
          statusOf r @?= 200
          mapM_ (\ext -> doesFileExist (old ext) >>= (@?= False)) [".jpg", ".jpeg", ".GIF"]
          BS.readFile (old ".png") >>= (@?= png)
      , testCase "uppercase .JPG replacement preserves uploaded JPEG on Windows" $
        withOpenServer "art-uppercase" $ \app cfg -> do
          (slug, endpoint) <- createArtworkPlaylist app
          jpeg <- BS.readFile "test/artwork/sample.jpg"
          BS.writeFile (scPlaylistsDir cfg </> T.unpack slug ++ ".JPG") jpeg
          uploaded <- run1 app (sreq methodPut endpoint "" (LBS.fromStrict jpeg) [])
          statusOf uploaded @?= 200
          downloaded <- run1 app (sreq methodGet endpoint "" "" [])
          statusOf downloaded @?= 200
          simpleBody downloaded @?= LBS.fromStrict jpeg
          names <- listDirectory (scPlaylistsDir cfg)
          length [n | n <- names, ".jpg" `T.isSuffixOf` T.toLower (T.pack n)] @?= 1
      , testCase "rename preserves artwork bytes and extension" $
        withOpenServer "art-rename" $ \app cfg -> do
          (slug, _) <- createArtworkPlaylist app
          gif <- BS.readFile "test/artwork/sample.gif"
          let old = scPlaylistsDir cfg </> T.unpack slug ++ ".gif"
              new = scPlaylistsDir cfg </> T.unpack renameSlug ++ ".gif"
          BS.writeFile old gif
          r <- run1 app (sreq methodPut ("/api/playlists/" <> TE.encodeUtf8 slug) "" (encode renameDto) [])
          statusOf r @?= 200
          doesFileExist old >>= (@?= False)
          BS.readFile new >>= (@?= gif)
      , testCase "delete and restore include sidecar, unrelated images retained" $
        withOpenServer "art-delete" $ \app cfg -> do
          (slug, endpoint) <- createArtworkPlaylist app
          png <- BS.readFile "test/artwork/sample.png"
          BS.writeFile (scPlaylistsDir cfg </> "unrelated.png") png
          uploaded <- run1 app (sreq methodPut endpoint "" (LBS.fromStrict png) [])
          statusOf uploaded @?= 200
          deleted <- run1 app (sreq methodDelete ("/api/playlists/" <> TE.encodeUtf8 slug) "" "" [])
          statusOf deleted @?= 200
          doesFileExist (scPlaylistsDir cfg </> T.unpack slug ++ ".png") >>= (@?= False)
          BS.readFile (scPlaylistsDir cfg </> "unrelated.png") >>= (@?= png)
          restored <- run1 app (sreq methodPost ("/api/trash/" <> TE.encodeUtf8 (trashIdOf deleted) <> "/restore") "" "" [])
          statusOf restored @?= 200
          BS.readFile (scPlaylistsDir cfg </> T.unpack slug ++ ".png") >>= (@?= png)
      , testCase "playlist without artwork and idempotent artwork deletion" $
        withOpenServer "art-none" $ \app _ -> do
          (_, endpoint) <- createArtworkPlaylist app
          missing <- run1 app (sreq methodGet endpoint "" "" [])
          statusOf missing @?= 404
          removed <- run1 app (sreq methodDelete endpoint "" "" [])
          statusOf removed @?= 200
      , testCase "Unicode playlist basename discovered without metadata" $
        withOpenServer "art-unicode" $ \app cfg -> do
          png <- BS.readFile "test/artwork/sample.png"
          let slug = "Любимые хиты"
              endpoint = "/api/playlists/" <> TE.encodeUtf8 slug <> "/artwork"
          BS.writeFile (scPlaylistsDir cfg </> T.unpack slug ++ ".nsp") (compiledNsp validDto)
          BS.writeFile (scPlaylistsDir cfg </> T.unpack slug ++ ".png") png
          r <- run1 app (sreq methodGet endpoint "" "" [])
          statusOf r @?= 200
          simpleBody r @?= LBS.fromStrict png
      , testCase "missing playlist cannot create artwork" $
        withOpenServer "art-missing" $ \app cfg -> do
          png <- BS.readFile "test/artwork/sample.png"
          r <- run1 app (sreq methodPut "/api/playlists/missing/artwork" "" (LBS.fromStrict png) [])
          statusOf r @?= 404
          listDirectory (scPlaylistsDir cfg) >>= (@?= [])
      , testCase "path traversal and Windows alternate streams rejected" $
        withOpenServer "art-path" $ \app cfg -> do
          _ <- createArtworkPlaylist app
          png <- BS.readFile "test/artwork/sample.png"
          mapM_ (\slug -> do
            readArtwork cfg slug >>= (\r -> assertBool "read traversal accepted" (isLeft r))
            putArtwork cfg slug png >>= (\r -> assertBool "upload traversal accepted" (isLeft r))
            deleteArtwork cfg slug >>= (\r -> assertBool "delete traversal accepted" (isLeft r)))
            ["../outside", "..\\outside", "C:\\outside", "playlist:stream", "/outside"]
          mapM_ (\p -> do
            r <- run1 app (sreq methodPut p "" (LBS.fromStrict png) [])
            assertBool "HTTP traversal accepted" (statusOf r >= 400))
            ["/api/playlists/../artwork", "/api/playlists/..%2Foutside/artwork", "/api/playlists/C:%5Coutside/artwork"]
      , testCase "rename cleanup failure rolls back playlist, state and artwork" $
        withOpenServer "art-rollback" $ \app cfg -> do
          (slug, _) <- createArtworkPlaylist app
          png <- BS.readFile "test/artwork/sample.png"
          let old = scPlaylistsDir cfg </> T.unpack slug ++ ".png"
          BS.writeFile old png
          before <- BS.readFile (stateFilePath cfg)
          r <- publishPlaylistWith (\p -> if p == scPlaylistsDir cfg </> T.unpack slug ++ ".nsp"
            then removeFile p else ioError (userError "injected cleanup failure")) cfg (Just slug) renameSlug False renameDto
          assertBool "rename unexpectedly succeeded" (isLeft r)
          BS.readFile old >>= (@?= png)
          BS.readFile (stateFilePath cfg) >>= (@?= before)
          doesFileExist (scPlaylistsDir cfg </> T.unpack slug ++ ".nsp") >>= (@?= True)
          doesFileExist (scPlaylistsDir cfg </> T.unpack renameSlug ++ ".nsp") >>= (@?= False)
          doesFileExist (scPlaylistsDir cfg </> T.unpack renameSlug ++ ".png") >>= (@?= False)
      , testCase "rename artwork target conflict leaves originals intact" $
        withOpenServer "art-conflict" $ \app cfg -> do
          (slug, _) <- createArtworkPlaylist app
          png <- BS.readFile "test/artwork/sample.png"
          BS.writeFile (scPlaylistsDir cfg </> T.unpack slug ++ ".png") png
          BS.writeFile (scPlaylistsDir cfg </> T.unpack renameSlug ++ ".jpg") "other image"
          r <- run1 app (sreq methodPut ("/api/playlists/" <> TE.encodeUtf8 slug) "" (encode renameDto) [])
          statusOf r @?= 409
          BS.readFile (scPlaylistsDir cfg </> T.unpack slug ++ ".png") >>= (@?= png)
          BS.readFile (scPlaylistsDir cfg </> T.unpack renameSlug ++ ".jpg") >>= (@?= "other image")
      , testCase "unsafe artwork directory blocks upload without losing old image" $
        withOpenServer "art-blocked" $ \app cfg -> do
          (slug, endpoint) <- createArtworkPlaylist app
          jpeg <- BS.readFile "test/artwork/sample.jpg"
          png <- BS.readFile "test/artwork/sample.png"
          BS.writeFile (scPlaylistsDir cfg </> T.unpack slug ++ ".jpg") jpeg
          createDirectory (scPlaylistsDir cfg </> T.unpack slug ++ ".png")
          r <- run1 app (sreq methodPut endpoint "" (LBS.fromStrict png) [])
          statusOf r @?= 409
          BS.readFile (scPlaylistsDir cfg </> T.unpack slug ++ ".jpg") >>= (@?= jpeg)
      ])
  where
    isLeft (Left _) = True
    isLeft _ = False

createArtworkPlaylist :: Application -> IO (Text, BS.ByteString)
createArtworkPlaylist app = do
  created <- run1 app (sreq methodPost "/api/playlists" "" (encode validDto) [])
  statusOf created @?= 201
  let slug = slugFromName (pdName validDto)
  pure (slug, "/api/playlists/" <> TE.encodeUtf8 slug <> "/artwork")
