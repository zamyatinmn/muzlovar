{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Необязательный адаптер Subsonic для честного статуса синхронизации.
--
-- Navidrome (v0.64.1) удаляет сущность smart-плейлиста только через
-- Subsonic @deletePlaylist@ или свой UI: исчезновение файла @.nsp@ из
-- каталога автоимпорта сущность из базы данных не удаляет. Поэтому
-- Muzlovar:
--
--   * при наличии @MUZLOVAR_SUBSONIC_URL@ умеет спрашивать
--     @getPlaylists@ и вызывать @deletePlaylist@;
--   * без этих переменных честно сообщает, что сущность нужно удалить
--     в Navidrome, и не притворяется, что база данных изменена.
--
-- Ошибки никогда не содержат пароль и строку запроса: 'scrubUrl'
-- вырезает userinfo и query из URL.
module Nspeller.Muzlovar.Subsonic
  ( SubsonicConfig (..)
  , SubsonicPlaylist (..)
  , loadSubsonicConfig
  , subsonicEnabled
  , subsonicGetPlaylists
  , subsonicDeletePlaylist
  , scrubUrl
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (Object, ToJSON (..), Value (..), eitherDecode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( HttpException
  , Request
  , Response
  , httpLbs
  , newManager
  , parseRequest
  , setQueryString
  , responseBody
  , responseStatus
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)

------------------------------------------------------------------------------
-- Конфигурация
------------------------------------------------------------------------------

-- | Реквизиты Subsonic-совместимого API.
data SubsonicConfig = SubsonicConfig
  { ssUrl :: Text
    -- ^ Базовый URL Navidrome, без завершающего слэша.
  , ssUser :: Text
  , ssPassword :: Text
  }
  deriving (Eq, Show)

-- | Плейлист из @getPlaylists@.
data SubsonicPlaylist = SubsonicPlaylist
  { spId :: Text
  , spName :: Text
  , spOwner :: Maybe Text
  , spPublic :: Maybe Bool
  , spSongCount :: Maybe Int
  }
  deriving (Eq, Show)

instance ToJSON SubsonicPlaylist where
  toJSON p =
    object
      [ "id" .= spId p
      , "name" .= spName p
      , "owner" .= spOwner p
      , "public" .= spPublic p
      , "songCount" .= spSongCount p
      ]

-- | Прочитать конфигурацию из окружения. @Nothing@ — адаптер выключен
-- (переменные не заданы или заданы не полностью).
loadSubsonicConfig :: IO (Maybe SubsonicConfig)
loadSubsonicConfig = do
  mUrl <- lookupEnv "MUZLOVAR_SUBSONIC_URL"
  mUser <- lookupEnv "MUZLOVAR_SUBSONIC_USER"
  mPass <- lookupEnv "MUZLOVAR_SUBSONIC_PASSWORD"
  pure $ case (mUrl, mUser, mPass) of
    (Just u, Just usr, Just p)
      | not (null u) && not (null usr) ->
          Just (SubsonicConfig (T.pack u) (T.pack usr) (T.pack p))
    _ -> Nothing

-- | Включён ли адаптер.
subsonicEnabled :: Maybe SubsonicConfig -> Bool
subsonicEnabled = maybe False (const True)

------------------------------------------------------------------------------
-- Вызовы API
------------------------------------------------------------------------------

-- | Список плейлистов. @Left@ — текст ошибки без учётных данных.
subsonicGetPlaylists :: SubsonicConfig -> IO (Either Text [SubsonicPlaylist])
subsonicGetPlaylists cfg = do
  r <- callSubsonic cfg "getPlaylists" []
  pure $ case r of
    Left e -> Left e
    Right v -> case parsePlaylists v of
      Nothing -> Left "Не удалось разобрать ответ Subsonic (getPlaylists)."
      Just xs -> Right xs

-- | Удалить плейлист по идентификатору Navidrome.
subsonicDeletePlaylist :: SubsonicConfig -> Text -> IO (Either Text ())
subsonicDeletePlaylist cfg pid = do
  r <- callSubsonic cfg "deletePlaylist" [("id", Just (TE.encodeUtf8 pid))]
  pure $ case r of
    Left e -> Left e
    Right _ -> Right ()

------------------------------------------------------------------------------
-- HTTP
------------------------------------------------------------------------------

-- | Общий вызов метода Subsonic.
callSubsonic ::
  SubsonicConfig ->
  Text ->
  [(BS.ByteString, Maybe BS.ByteString)] ->
  IO (Either Text Value)
callSubsonic cfg method extra = do
  let url = T.unpack (ssUrl cfg <> "/" <> method)
  reqE <- try (parseRequest url) :: IO (Either HttpException Request)
  case reqE of
    Left _ -> pure (Left "Некорректный URL Subsonic.")
    Right req0 -> do
      let query =
            [ ("u", Just (TE.encodeUtf8 (ssUser cfg)))
            , ("p", Just (TE.encodeUtf8 (ssPassword cfg)))
            , ("v", Just "1.16.1")
            , ("c", Just "muzlovar")
            , ("f", Just "json")
            ]
              <> extra
          req = setQueryString query req0
      mgr <- newManager tlsManagerSettings
      resp <- try (httpLbs req mgr) :: IO (Either IOException (Response LBS.ByteString))
      pure $ case resp of
        Left _ -> Left ("Не удалось подключиться к Navidrome: " <> scrubUrl (ssUrl cfg))
        Right r ->
          let code = statusCode (responseStatus r)
           in if code /= 200
                then
                  Left
                    ( "Subsonic вернул статус "
                        <> T.pack (show code)
                        <> " ("
                        <> scrubUrl (ssUrl cfg)
                        <> ")"
                    )
                else decodeEnvelope (responseBody r)

-- | Разбор @{\"subsonic-response\":{...}}@: ошибка сервера превращается
-- в текст, успех — в значение ответа.
decodeEnvelope :: LBS.ByteString -> Either Text Value
decodeEnvelope body = case (eitherDecode body :: Either String Value) of
  Left _ -> Left "Subsonic вернул не-JSON ответ."
  Right v -> case v of
    Object o -> case KM.lookup "subsonic-response" o of
      Just (Object inner) -> case KM.lookup "status" inner of
        Just (String s)
          | s == "ok" -> Right (Object inner)
        _ -> Left (errorMessage inner)
      _ -> Left "В ответе Subsonic нет объекта subsonic-response."
    _ -> Left "Неожиданный формат ответа Subsonic."

errorMessage :: Object -> Text
errorMessage inner = case KM.lookup "message" inner of
  Just (String m) -> m
  _ -> case KM.lookup "error" inner of
    Just (Object e) -> case KM.lookup "message" e of
      Just (String m) -> m
      _ -> "Ошибка Subsonic."
    _ -> "Ошибка Subsonic."

------------------------------------------------------------------------------
-- Парсеры
------------------------------------------------------------------------------

-- | Разбор тела ответа @getPlaylists@ (уже внутри @subsonic-response@).
parsePlaylists :: Value -> Maybe [SubsonicPlaylist]
parsePlaylists = \case
  Object resp -> case KM.lookup "playlists" resp of
    Just (Object pl) -> case KM.lookup "playlist" pl of
      Just (Array arr) -> mapM parseOne (foldr (:) [] arr)
      Just (Object single) -> fmap (: []) (parseOne (Object single))
      _ -> Just []
    _ -> Just []
  _ -> Nothing
  where
    parseOne :: Value -> Maybe SubsonicPlaylist
    parseOne v = case v of
      Object o ->
        case (KM.lookup "id" o, KM.lookup "name" o) of
          (Just (String i), Just (String n)) ->
            Just
              SubsonicPlaylist
                { spId = i
                , spName = n
                , spOwner = textAt "owner" o
                , spPublic = boolAt "public" o
                , spSongCount = intAt "songCount" o
                }
          _ -> Nothing
      _ -> Nothing

    textAt k o = case KM.lookup k o of
      Just (String t) -> Just t
      _ -> Nothing
    boolAt k o = case KM.lookup k o of
      Just (Bool b) -> Just b
      _ -> Nothing
    intAt k o = case KM.lookup k o of
      Just (Number n) ->
        let i = round (toRational n) :: Integer
         in if fromInteger i == toRational n then Just (fromInteger i) else Nothing
      _ -> Nothing

------------------------------------------------------------------------------
-- Безопасность логов
------------------------------------------------------------------------------

-- | Убрать из URL всё, что может содержать учётные данные: userinfo
-- (@user:pass\@@), строку запроса и фрагмент.
scrubUrl :: Text -> Text
scrubUrl u =
  let noFrag = T.takeWhile (/= '#') (T.takeWhile (/= '?') u)
      (scheme, rest0) = case T.breakOn "://" noFrag of
        (s, r)
          | not (T.null r) -> (s <> "://", T.drop 3 r)
        _ -> ("", noFrag)
      authority = T.takeWhile (/= '/') rest0
      path = T.drop (T.length authority) rest0
      cleanAuthority = case T.breakOnEnd "@" authority of
        (_, after)
          | not (T.null after) -> after
        _ -> authority
   in scheme <> cleanAuthority <> path
