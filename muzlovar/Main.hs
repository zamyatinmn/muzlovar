{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Точка входа веб-редактора Muzlovar.
--
-- Переменные окружения:
--
--   * @MUZLOVAR_USERNAME@ и @MUZLOVAR_PASSWORD@ — необязательны:
--     обе заданы — включается Basic Auth; обе не заданы (или пустые) —
--     авторизации нет; задана только одна — приложение не стартует;
--   * @MUZLOVAR_RULES_DIR@, @MUZLOVAR_PLAYLISTS_DIR@,
--     @MUZLOVAR_TRASH_DIR@ — каталоги (по умолчанию @./rules@,
--     @./playlists@, @./trash@);
--   * @MUZLOVAR_PORT@ (по умолчанию 8765) и @MUZLOVAR_HOST@
--     (по умолчанию все интерфейсы);
--   * @MUZLOVAR_SUBSONIC_URL@, @MUZLOVAR_SUBSONIC_USER@,
--     @MUZLOVAR_SUBSONIC_PASSWORD@ — необязательный адаптер
--     Subsonic для честного статуса синхронизации.
--
-- Пароль и заголовок Authorization не пишутся в stdout/stderr.
module Main (main) where

import Data.String (fromString)
import qualified Data.Text as T
import Data.Text (Text)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setHost
  , setInstallShutdownHandler
  , setPort
  )
import Nspeller.Muzlovar.Server (ServerConfig (..), muzlovarApp)
import Nspeller.Muzlovar.Store (StoreConfig (..), ensureStoreDirs, storeErrorMessage)
import Nspeller.Muzlovar.Subsonic (loadSubsonicConfig)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, hPutStrLn, stderr, stdout, utf8)
import Text.Read (readMaybe)
#ifndef mingw32_HOST_OS
import Control.Monad (void)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
#endif

main :: IO ()
main = do
  -- Локаль в минимальных образах бывает POSIX/ASCII: без явной UTF-8
  -- первая же кириллическая строка в stderr уронит процесс (hPutChar).
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  auth <- readAuthConfig

  rulesDir <- envWith "MUZLOVAR_RULES_DIR" "./rules"
  playlistsDir <- envWith "MUZLOVAR_PLAYLISTS_DIR" "./playlists"
  trashDir <- envWith "MUZLOVAR_TRASH_DIR" "./trash"
  portText <- envWith "MUZLOVAR_PORT" "8765"
  host <- envWith "MUZLOVAR_HOST" "*"

  port <- case readMaybe (T.unpack portText) :: Maybe Int of
    Just p
      | p > 0 && p < 65536 -> pure p
    _ -> do
      hPutStrLn stderr ("MUZLOVAR_PORT должен быть числом от 1 до 65535, получено: " ++ T.unpack portText)
      exitFailure

  let storeCfg =
        StoreConfig
          { scRulesDir = T.unpack rulesDir
          , scPlaylistsDir = T.unpack playlistsDir
          , scTrashDir = T.unpack trashDir
          }

  ensured <- ensureStoreDirs storeCfg
  case ensured of
    Left e -> do
      hPutStrLn stderr (T.unpack (storeErrorMessage e))
      exitFailure
    Right () -> pure ()

  subsonic <- loadSubsonicConfig

  app <- muzlovarApp (ServerConfig storeCfg subsonic auth)

  let warpSettings =
        setInstallShutdownHandler installShutdown $
          setHost (fromString (T.unpack host)) $
            setPort port defaultSettings

  hPutStrLn stderr $
    "Muzlovar слушает порт "
      ++ show port
      ++ " (каталог правил: "
      ++ T.unpack rulesDir
      ++ ", плейлисты: "
      ++ T.unpack playlistsDir
      ++ ", корзина: "
      ++ T.unpack trashDir
      ++ ")"
  case auth of
    Nothing -> hPutStrLn stderr "Авторизация выключена: MUZLOVAR_USERNAME/MUZLOVAR_PASSWORD не заданы."
    Just _ -> hPutStrLn stderr "Авторизация: Basic Auth включён."
  case subsonic of
    Nothing ->
      hPutStrLn stderr $
        "Subsonic не настроен: файлы .nsp будут удаляться в корзину, "
          ++ "а сущность в Navidrome останется до ручного удаления."
    Just _ -> hPutStrLn stderr "Subsonic настроен: сущность будет удалена через API Navidrome."

  runSettings warpSettings app

------------------------------------------------------------------------------
-- Окружение
------------------------------------------------------------------------------

-- | Необязательные учётные данные Basic Auth.
--
-- Обе переменные заданы (и непустые) — авторизация включается;
-- обе не заданы (или пустые) — авторизация выключена;
-- задана ровно одна — ошибка конфигурации, приложение не стартует.
readAuthConfig :: IO (Maybe (Text, Text))
readAuthConfig = do
  mu <- lookupEnv "MUZLOVAR_USERNAME"
  mp <- lookupEnv "MUZLOVAR_PASSWORD"
  let nu = nonEmpty mu
      np = nonEmpty mp
  case (nu, np) of
    (Just u, Just p) -> pure (Just (T.pack u, T.pack p))
    (Nothing, Nothing) -> pure Nothing
    _ -> do
      hPutStrLn stderr
        "Задана только одна из MUZLOVAR_USERNAME/MUZLOVAR_PASSWORD: укажите обе или ни одной."
      exitFailure
  where
    nonEmpty = maybe Nothing (\x -> if null x then Nothing else Just x)

-- | Переменная окружения со значением по умолчанию.
envWith :: String -> Text -> IO Text
envWith name def = do
  v <- lookupEnv name
  pure $ case v of
    Just x
      | not (null x) -> T.pack x
    _ -> def

------------------------------------------------------------------------------
-- Корректное завершение
------------------------------------------------------------------------------

-- | На POSIX Warp закрывает слушающий сокет и дообрабатывает
-- уже принятые соединения; подшиваем обработчики SIGTERM/SIGINT к
-- действию, которое Warp передаёт в этот обработчик. На Windows
-- сигналов нет — хендлер пустой, корректный останов там обеспечивает
-- `tini` и закрытие консоли.
installShutdown :: IO () -> IO ()
#ifdef mingw32_HOST_OS
installShutdown _ = pure ()
#else
installShutdown shutdownAction = do
  _ <- installHandler sigTERM (Catch shutdownAction) Nothing
  void (installHandler sigINT (Catch shutdownAction) Nothing)
#endif
