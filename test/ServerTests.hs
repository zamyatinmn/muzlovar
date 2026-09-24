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
import System.Directory (doesFileExist, listDirectory)
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
    ]
