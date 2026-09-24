{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HTTP-слой Muzlovar (Scotty).
--
-- Маршруты:
--
--   * @GET /health@ — публичный, без авторизации;
--   * @GET /api/schema@ — единый источник схемы полей и операторов;
--   * @GET\/POST\/PUT\/DELETE /api/playlists[\/:slug]@;
--   * @POST /api/validate@ — проверка DTO без записи на диск;
--   * @GET /api/trash@, @POST /api/trash\/:id\/restore@,
--     @DELETE /api/trash\/:id@;
--   * HTML-страницы @/@, @/new@, @/edit\/:slug@, @/trash@;
--   * @/static/*@ — вшитые CSS/JS, логотип и фавикон; @/favicon.ico@ —
--     alias фавикона (браузер запрашивает его без Credentials).
--
-- Basic Auth с сравнением пароля за постоянное время (включается только
-- при заданных @MUZLOVAR_USERNAME@ и @MUZLOVAR_PASSWORD@); пароль и
-- заголовок Authorization никуда не логируются и не попадают в ответы.
module Nspeller.Muzlovar.Server
  ( ServerConfig (..)
  , muzlovarApp
  , basicAuthMiddleware
  , constantTimeEq
  , storeErrorToApi
  , storeErrorStatus
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, eitherDecode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Bits (xor, (.|.))
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Word (Word8)
import Lucid.Base (Html)
import Network.HTTP.Types.Header (hAuthorization, hWWWAuthenticate)
import Network.HTTP.Types.Status
  ( Status
  , status200
  , status201
  , status401
  , status404
  , status409
  , status422
  , status500
  )
import Network.Wai (Application, Middleware, pathInfo, requestHeaders, responseLBS)
import qualified Nspeller.Muzlovar.Assets as Assets
import Nspeller.Muzlovar.Html (editorPage, errorPage, indexPage, renderHtml, trashPage)
import Nspeller.Muzlovar.Store
  ( PlaylistDetail (pdEntry, pdRaw)
  , PlaylistEntry (peNspFile, peTitle)
  , StoreConfig (..)
  , StoreError (..)
  , deletePlaylistFiles
  , listPlaylists
  , listTrash
  , publishPlaylist
  , publishPlaylistFrom
  , purgeTrash
  , readPlaylist
  , restoreTrash
  , slugFromName
  , storeErrorMessage
  )
import Nspeller.Muzlovar.Subsonic
  ( SubsonicConfig
  , SubsonicPlaylist (spId, spName)
  , subsonicDeletePlaylist
  , subsonicGetPlaylists
  )
import Nspeller.Muzlovar.Types
  ( ApiError
  , Compiled (cmpMix, cmpNsp)
  , PlaylistDto
  , apiError
  , compilePlaylistDto
  , pdName
  )
import Nspeller.Schema (schemaJson)
import System.FilePath ((</>))
import Web.Scotty
  ( ActionM
  , ScottyM
  , body
  , delete
  , get
  , json
  , notFound
  , pathParam
  , post
  , put
  , queryParamMaybe
  , raw
  , request
  , scottyApp
  , setHeader
  , status
  )

------------------------------------------------------------------------------
-- Конфигурация
------------------------------------------------------------------------------

-- | Всё, что нужно серверу.
data ServerConfig = ServerConfig
  { scStoreCfg :: StoreConfig
  , scSubsonicCfg :: Maybe SubsonicConfig
  , scAuth :: Maybe (Text, Text)
    -- ^ Учётные данные Basic Auth: @Just (логин, пароль)@ — включает
    -- авторизацию, @Nothing@ — сервис открыт (внутренняя сеть).
  }

-- | WAI-приложение: маршруты Scotty под Basic Auth middleware
-- (middleware подключается, только если 'scAuth' задан).
muzlovarApp :: ServerConfig -> IO Application
muzlovarApp cfg = do
  app <- scottyApp (routes cfg)
  pure $ case scAuth cfg of
    Nothing -> app
    Just (user, pass) -> basicAuthMiddleware user pass app

------------------------------------------------------------------------------
-- Авторизация
------------------------------------------------------------------------------

-- | Basic Auth middleware: @/health@, @/static/*@ и @/favicon.ico@
-- открыты, остальное — только при верных учётных данных.
basicAuthMiddleware :: Text -> Text -> Middleware
basicAuthMiddleware user pass app req respond =
  if isPublic (pathInfo req)
    then app req respond
    else
      if authorized (requestHeaders req)
        then app req respond
        else
          respond $
            responseLBS
              status401
              [(hWWWAuthenticate, "Basic realm=\"Muzlovar\", charset=\"UTF-8\"")]
              "{\"error\":{\"code\":\"unauthorized\",\"message\":\"Требуется авторизация.\",\"path\":null,\"line\":null,\"column\":null}}"
  where
    isPublic p = p == ["health"] || p == ["favicon.ico"] || take 1 p == ["static"]

    authorized hdrs =
      case lookup hAuthorization hdrs of
        Nothing -> False
        Just rawHeader ->
          case BS.breakSubstring " " rawHeader of
            (scheme, rest)
              | BS.map lowerByte scheme == "basic" ->
                  case Base64.decode (BS.drop 1 rest) of
                    Left _ -> False
                    Right cred ->
                      case BS.breakSubstring ":" cred of
                        (u, p) ->
                          constantTimeEq u (TE.encodeUtf8 user)
                            && constantTimeEq (BS.drop 1 p) (TE.encodeUtf8 pass)
            _ -> False

lowerByte :: Word8 -> Word8
lowerByte c
  | c >= 65 && c <= 90 = c + 32
  | otherwise = c

-- | Сравнение двух байтовых строк за постоянное время: результат не
-- зависит от позиции первого различия (длины сверяются после свёртки).
constantTimeEq :: BS.ByteString -> BS.ByteString -> Bool
constantTimeEq a b =
  let n = max (BS.length a) (BS.length b)
      pa = a <> BS.replicate (n - BS.length a) 0
      pb = b <> BS.replicate (n - BS.length b) 0
      diff =
        foldl'
          (\acc (x, y) -> acc .|. (fromIntegral x `xor` fromIntegral y))
          (0 :: Int)
          (zip (BS.unpack pa) (BS.unpack pb))
   in diff == 0 && BS.length a == BS.length b

------------------------------------------------------------------------------
-- Ошибки
------------------------------------------------------------------------------

-- | Перевод ошибки хранилища в пару «структурированные ошибки + статус».
storeErrorToApi :: StoreError -> ([ApiError], Status)
storeErrorToApi = \case
  StoreNotFound s ->
    ([apiError "not_found" (storeErrorMessage (StoreNotFound s))], status404)
  StoreUnsafeSlug s ->
    ([apiError "unsafe_slug" (storeErrorMessage (StoreUnsafeSlug s))], status404)
  StoreUnsafePath p ->
    ([apiError "unsafe_path" (storeErrorMessage (StoreUnsafePath p))], status422)
  StoreConflict s ->
    ([apiError "conflict" (storeErrorMessage (StoreConflict s))], status409)
  StoreInvalid _ errs -> (errs, status422)
  StoreIo m -> ([apiError "io_error" m], status500)
  StorePartial m -> ([apiError "partial_delete" m], status500)
  StoreCleanupBlocked p why ->
    ( [apiError "cleanup_blocked" (storeErrorMessage (StoreCleanupBlocked p why))]
    , status422
    )
  StoreCleanupFailed p why ->
    ( [apiError "cleanup_failed" (storeErrorMessage (StoreCleanupFailed p why))]
    , status500
    )

-- | HTTP-статус ошибки хранилища.
storeErrorStatus :: StoreError -> Status
storeErrorStatus = snd . storeErrorToApi

-- | Ответ с перечнем ошибок в общем для API формате.
respondErrors :: Status -> [ApiError] -> ActionM ()
respondErrors st errs = do
  status st
  json $
    object
      [ "errors" .= errs
      , "error" .= fromMaybe (apiError "error" "Неизвестная ошибка.") (listToMaybe errs)
      ]

respondStoreError :: StoreError -> ActionM ()
respondStoreError e = respondErrors (storeErrorStatus e) (fst (storeErrorToApi e))

------------------------------------------------------------------------------
-- Маршруты
------------------------------------------------------------------------------

routes :: ServerConfig -> ScottyM ()
routes cfg = do
  --------------------------------------------------------------- health
  get "/health" $ do
    status status200
    json (object ["status" .= ("ok" :: Text)])

  ------------------------------------------------------------------- api
  get "/api/schema" $
    json schemaJson

  get "/api/playlists" $ do
    r <- liftIO (listPlaylists (scStoreCfg cfg))
    either respondStoreError (\xs -> json (object ["playlists" .= xs])) r

  post "/api/validate" $ do
    parsed <- parseDtoBody
    case parsed of
      Left errs -> respondErrors status422 errs
      Right dto -> case compilePlaylistDto dto of
        Left errs -> respondErrors status422 errs
        Right compiled ->
          json $
            object
              [ "ok" .= True
              , -- Итоговый filename из текущего названия: клиент
                -- сравнивает его с опубликованным путём и показывает
                -- блок «Опубликовано / Будет опубликовано».
                "slug" .= slugFromName (pdName dto)
              , "mix" .= LT.fromStrict (cmpMix compiled)
              , "nsp" .= LTE.decodeUtf8 (cmpNsp compiled)
              , "errors" .= ([] :: [ApiError])
              ]

  post "/api/playlists" $ do
    parsed <- parseDtoBody
    case parsed of
      Left errs -> respondErrors status422 errs
      Right dto -> do
        overwrite <- overwriteRequested
        let slug = slugFromName (pdName dto)
        r <- liftIO (publishPlaylist (scStoreCfg cfg) slug overwrite dto)
        case r of
          Left e -> respondStoreError e
          Right d -> do
            status status201
            json d

  get "/api/playlists/:slug" $ do
    slug <- pathParam "slug"
    r <- liftIO (readPlaylist (scStoreCfg cfg) slug)
    either respondStoreError json r

  put "/api/playlists/:slug" $ do
    slug <- pathParam "slug"
    detail <- liftIO (readPlaylist (scStoreCfg cfg) slug)
    case detail of
      Left e -> respondStoreError e
      Right d
        | pdRaw d ->
            respondErrors
              status409
              [ apiError
                  "external_readonly"
                  "Внешняя подборка содержит конструкции, неизвестные редактору, и доступна только для чтения."
              ]
        | otherwise -> do
            parsed <- parseDtoBody
            case parsed of
              Left errs -> respondErrors status422 errs
              Right dto -> do
                overwrite <- overwriteRequested
                -- Итоговый slug — из названия в DTO: переименование
                -- меняет filename. Прежний slug (адрес запроса) —
                -- identity подборки для безопасного cleanup старых
                -- файлов по persisted state.
                let target = slugFromName (pdName dto)
                r <-
                  liftIO
                    (publishPlaylistFrom (scStoreCfg cfg) (Just slug) target overwrite dto)
                either respondStoreError json r

  delete "/api/playlists/:slug" $ do
    slug <- pathParam "slug"
    detail <- liftIO (readPlaylist (scStoreCfg cfg) slug)
    case detail of
      Left e -> respondStoreError e
      Right d -> do
        r <- liftIO (deletePlaylistFiles (scStoreCfg cfg) slug)
        case r of
          Left e -> respondStoreError e
          Right tid -> do
            sub <- liftIO (subsonicDeleteNamed (scSubsonicCfg cfg) (peTitle (pdEntry d)))
            json (object ["trashId" .= tid, "subsonic" .= sub])

  ----------------------------------------------------------------- trash
  get "/api/trash" $ do
    r <- liftIO (listTrash (scStoreCfg cfg))
    either respondStoreError (\xs -> json (object ["trash" .= xs])) r

  post "/api/trash/:id/restore" $ do
    tid <- pathParam "id"
    r <- liftIO (restoreTrash (scStoreCfg cfg) tid)
    either respondStoreError (const (json (object ["restored" .= True]))) r

  delete "/api/trash/:id" $ do
    tid <- pathParam "id"
    r <- liftIO (purgeTrash (scStoreCfg cfg) tid)
    either respondStoreError (const (json (object ["purged" .= True]))) r

  ---------------------------------------------------------------- static
  get "/static/muzlovar.css" $
    serveBytes "text/css; charset=utf-8" Assets.muzlovarCss
  get "/static/muzlovar.js" $
    serveBytes "application/javascript; charset=utf-8" Assets.muzlovarJs
  get "/static/logo.png" $
    serveBytes "image/png" Assets.logoPng
  get "/static/favicon.ico" $
    serveBytes "image/x-icon" Assets.faviconIco
  -- Браузер запрашивает /favicon.ico без Credentials: без этого
  -- маршрута (и без публикации в basicAuthMiddleware) вкладка
  -- осталась бы без иконки при включённой авторизации.
  get "/favicon.ico" $
    serveBytes "image/x-icon" Assets.faviconIco

  ------------------------------------------------------------------ html
  get "/" $ do
    r <- liftIO (listPlaylists (scStoreCfg cfg))
    case r of
      Left e -> htmlPage (errorPage (storeErrorMessage e))
      Right xs -> htmlPage (indexPage xs)

  get "/new" $
    htmlPage (editorPage Nothing Nothing (publishDir cfg))

  get "/edit/:slug" $ do
    slug <- pathParam "slug"
    r <- liftIO (readPlaylist (scStoreCfg cfg) slug)
    case r of
      Left e -> htmlPage (errorPage (storeErrorMessage e))
      Right d ->
        htmlPage
          ( editorPage
              (Just slug)
              ( actualPublishedPath cfg (peNspFile (pdEntry d)) )
              (publishDir cfg)
          )

  get "/trash" $ do
    r <- liftIO (listTrash (scStoreCfg cfg))
    case r of
      Left e -> htmlPage (errorPage (storeErrorMessage e))
      Right xs -> htmlPage (trashPage xs)

  notFound $ do
    p <- pathInfo <$> request
    if take 1 p == ["api"]
      then respondErrors status404 [apiError "not_found" "Маршрут не найден."]
      else htmlPage (errorPage "Страница не найдена.")

------------------------------------------------------------------------------
-- Вспомогательное
------------------------------------------------------------------------------

-- | Настроенный каталог публикации (.nsp): редактор показывает его в
-- блоке «Путь публикации» (до публикации — только каталог, после —
-- каталог с именем файла).
publishDir :: ServerConfig -> FilePath
publishDir = scPlaylistsDir . scStoreCfg

-- | Фактический путь опубликованного @.nsp@ для блока «Путь
-- публикации» (Nothing — подборка не опубликована): каталог
-- публикации плюс имя реально существующего файла из entry.
actualPublishedPath :: ServerConfig -> Maybe String -> Maybe FilePath
actualPublishedPath cfg = fmap (publishDir cfg </>)

-- | Отдать HTML-страницу.
htmlPage :: Html () -> ActionM ()
htmlPage page = do
  setHeader "Content-Type" "text/html; charset=utf-8"
  raw (LTE.encodeUtf8 (renderHtml page))

-- | Отдать вшитый байтовый ресурс.
serveBytes :: LT.Text -> BS.ByteString -> ActionM ()
serveBytes ct bs = do
  setHeader "Content-Type" ct
  setHeader "Cache-Control" "no-cache"
  raw (LBS.fromStrict bs)

-- | Разбор тела запроса как 'PlaylistDto' со структурированной ошибкой.
parseDtoBody :: ActionM (Either [ApiError] PlaylistDto)
parseDtoBody = do
  b <- body
  pure $ case eitherDecode b of
    Left e ->
      Left
        [ apiError
            "bad_json"
            ("Некорректный JSON тела запроса: " <> T.pack (take 200 e))
        ]
    Right d -> Right d

-- | Запрошена ли перезапись (@?overwrite=1@ и подобные).
overwriteRequested :: ActionM Bool
overwriteRequested = do
  v <- queryParamMaybe "overwrite" :: ActionM (Maybe Text)
  pure (maybe False (`elem` ["1", "true", "yes", "on"]) v)

------------------------------------------------------------------------------
-- Subsonic: честный статус синхронизации
------------------------------------------------------------------------------

-- | Удалить сущность в Navidrome (если адаптер настроен) и вернуть
-- честный статус. Без настройки 'attempted' = False: UI сообщает, что
-- сущность нужно удалить в Navidrome вручную.
subsonicDeleteNamed :: Maybe SubsonicConfig -> Text -> IO Value
subsonicDeleteNamed Nothing _ =
  pure $
    object
      [ "attempted" .= False
      , "message"
          .= ("Subsonic не настроен: сущность подборки нужно удалить в Navidrome вручную." :: Text)
      ]
subsonicDeleteNamed (Just c) name = do
  listed <- subsonicGetPlaylists c
  case listed of
    Left e -> pure (object ["attempted" .= True, "ok" .= False, "message" .= e])
    Right xs ->
      case find ((== name) . spName) xs of
        Nothing ->
          pure $
            object
              [ "attempted" .= True
              , "ok" .= True
              , "message"
                  .= ("В Navidrome нет подборки с таким названием — синхронизировать нечего." :: Text)
              ]
        Just p -> do
          deleted <- subsonicDeletePlaylist c (spId p)
          pure $ case deleted of
            Left e -> object ["attempted" .= True, "ok" .= False, "message" .= e]
            Right () -> object ["attempted" .= True, "ok" .= True]
