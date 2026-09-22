{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Файловое хранилище Muzlovar: список подборок, чтение, публикация,
-- удаление в корзину, восстановление и очистка корзины.
--
-- Каталоги (настраиваются окружением):
--
--   * правила @.mix@ — @MUZLOVAR_RULES_DIR@;
--   * опубликованные плейлисты @.nsp@ — @MUZLOVAR_PLAYLISTS_DIR@;
--   * корзина — @MUZLOVAR_TRASH_DIR@.
--
-- Публикация готовит оба файла целиком (компиляция DTO → канонический
-- @.mix@ → @.nsp@), затем кладёт их во временные файлы рядом с целевыми
-- и выполняет замещение через 'renameFile' с резервной копией. При любой
-- ошибке выполняется откат уже заменённых файлов.
--
-- Безопасность путей:
--
--   * slug допускает только строчные латинские буквы, цифры и дефисы
--     (без «..», разделителей, абсолютных путей) — 'slugSafe';
--   * перед записью целевой файл проверяется на символические ссылки и
--     на принадлежность каталогу ('checkTarget');
--   * чтение чужих файлов ограничено 'slugReadable' (один компонент пути).
module Nspeller.Muzlovar.Store
  ( -- * Конфигурация
    StoreConfig (..)
  , ensureStoreDirs

    -- * Список и чтение
  , PlaylistEntry (..)
  , PlaylistDetail (..)
  , listPlaylists
  , readPlaylist

    -- * Изменения
  , publishPlaylist
  , deletePlaylistFiles

    -- * Корзина
  , TrashEntry (..)
  , listTrash
  , restoreTrash
  , purgeTrash

    -- * Ошибки
  , StoreError (..)
  , storeErrorCode
  , storeErrorMessage

    -- * Помощники (используются Server и тестами)
  , containsRawDto
  , slugSafe
  , slugReadable
  , slugFromName
  , sortTextOf
  , summarizeDto
  ) where

import Control.Applicative (asum, (<|>))
import Control.Exception (IOException, try)
import Control.Monad (filterM, forM, forM_, void, when)
import Data.Aeson (ToJSON (..), Value (..), eitherDecode, object, (.=))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Nspeller.Muzlovar.Types
  ( ApiError (..)
  , Compiled (..)
  , CondDto (..)
  , GroupDto (..)
  , ItemDto (..)
  , PlaylistDto (..)
  , SortDto (..)
  , SortItemDto (..)
  , compilePlaylistDto
  , dtoFromMix
  , dtoToParsed
  , nspToDto
  )
import Nspeller.Render (renderParsedFile)
import Nspeller.Schema (FieldSchema (..), OperatorSchema (..), fieldSchemas, operatorSchemas)
import System.Directory
  ( canonicalizePath
  , createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getModificationTime
  , listDirectory
  , pathIsSymbolicLink
  , removeDirectory
  , removeDirectoryRecursive
  , removeFile
  , renameFile
  )
import System.FilePath
  ( (</>)
  , splitDirectories
  , takeBaseName
  , takeDirectory
  , takeExtension
  , takeFileName
  )

------------------------------------------------------------------------------
-- Конфигурация
------------------------------------------------------------------------------

-- | Конфигурация каталогов хранилища.
data StoreConfig = StoreConfig
  { scRulesDir :: FilePath
    -- ^ Каталог правил @.mix@.
  , scPlaylistsDir :: FilePath
    -- ^ Каталог плейлистов Navidrome @.nsp@.
  , scTrashDir :: FilePath
    -- ^ Каталог корзины.
  }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Ошибки
------------------------------------------------------------------------------

-- | Ошибки хранилища.
data StoreError
  = StoreNotFound Text
    -- ^ Подборка (или запись корзины) не найдена.
  | StoreUnsafeSlug Text
    -- ^ Slug не проходит проверку допустимости.
  | StoreUnsafePath FilePath
    -- ^ Целевой файл — символическая ссылка или лежит вне каталога.
  | StoreConflict Text
    -- ^ Файлы уже существуют, требуется явное подтверждение.
  | StoreInvalid Text [ApiError]
    -- ^ Подборка не прошла валидацию ядра.
  | StoreIo Text
    -- ^ Ошибка ввода-вывода.
  | StorePartial Text
    -- ^ Частичный перенос в корзину: часть файлов вернуть не удалось.
  deriving (Eq, Show)

-- | Код ошибки API для 'StoreError'.
storeErrorCode :: StoreError -> Text
storeErrorCode = \case
  StoreNotFound{} -> "not_found"
  StoreUnsafeSlug{} -> "unsafe_slug"
  StoreUnsafePath{} -> "unsafe_path"
  StoreConflict{} -> "conflict"
  StoreInvalid{} -> "validation_failed"
  StoreIo{} -> "io_error"
  StorePartial{} -> "partial_delete"

-- | Русское сообщение для 'StoreError'.
storeErrorMessage :: StoreError -> Text
storeErrorMessage = \case
  StoreNotFound s -> "Подборка «" <> s <> "» не найдена."
  StoreUnsafeSlug s -> "Недопустимое имя файла подборки: «" <> s <> "»."
  StoreUnsafePath p ->
    "Небезопасный целевой файл: "
      <> T.pack p
      <> " (символическая ссылка или выход за пределы каталога)."
  StoreConflict s ->
    "Файлы подборки «"
      <> s
      <> "» уже существуют — требуется явное подтверждение перезаписи."
  StoreInvalid s es ->
    "Подборка «" <> s <> "» не прошла валидацию: " <> apiErrorsText es
  StoreIo m -> m
  StorePartial m -> m

-- | Склеенные сообщения ошибок компиляции (со строкой и столбцом).
apiErrorsText :: [ApiError] -> Text
apiErrorsText es = T.intercalate "; " (map one es)
  where
    one e =
      aeMessage e
        <> case (aeLine e, aeColumn e) of
          (Just l, Just c) -> " (строка " <> tshow l <> ", столбец " <> tshow c <> ")"
          (Just l, _) -> " (строка " <> tshow l <> ")"
          _ -> ""

------------------------------------------------------------------------------
-- Slug
------------------------------------------------------------------------------

-- | Строгая проверка slug при записи: только строчные латинские буквы,
-- цифры и дефисы; без «..», разделителей, абсолютных путей и ведущего
-- дефиса. Именно эта проверка стоит на пути создания новых файлов.
slugSafe :: Text -> Bool
slugSafe slug =
  not (T.null slug)
    && T.length slug <= 128
    && not (T.isPrefixOf "-" slug)
    && T.all isOk slug
  where
    isOk c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'

-- | Проверка slug для чтения существующих файлов: имя должно быть одним
-- компонентом пути (без разделителей, «..», точек в начале).
slugReadable :: Text -> Bool
slugReadable slug =
  not (T.null slug)
    && T.length slug <= 255
    && not (".." `T.isInfixOf` slug)
    && not ("/" `T.isInfixOf` slug)
    && not ("\\" `T.isInfixOf` slug)
    && not (T.isPrefixOf "." slug)

-- | Транслитерация кириллицы для slug.
cyrillicTranslit :: [(Char, Text)]
cyrillicTranslit =
  [ ('а', "a"), ('б', "b"), ('в', "v"), ('г', "g"), ('д', "d")
  , ('е', "e"), ('ё', "e"), ('ж', "zh"), ('з', "z"), ('и', "i")
  , ('й', "y"), ('к', "k"), ('л', "l"), ('м', "m"), ('н', "n")
  , ('о', "o"), ('п', "p"), ('р', "r"), ('с', "s"), ('т', "t")
  , ('у', "u"), ('ф', "f"), ('х', "h"), ('ц', "c"), ('ч', "ch")
  , ('ш', "sh"), ('щ', "sch"), ('ъ', ""), ('ы', "y"), ('ь', "")
  , ('э', "e"), ('ю', "yu"), ('я', "ya")
  ]

-- | Slug из названия подборки: нижний регистр, транслитерация
-- кириллицы, пробелы и знаки препинания → дефис, схлопывание и обрезка
-- дефисов. Результат всегда проходит 'slugSafe'.
slugFromName :: Text -> Text
slugFromName title =
  let lowered = T.toLower (T.strip title)
      translit =
        T.concatMap (\c -> fromMaybe (T.singleton c) (lookup c cyrillicTranslit)) lowered
      step c
        | c >= 'a' && c <= 'z' = T.singleton c
        | c >= '0' && c <= '9' = T.singleton c
        | c `T.elem` " _-.," = "-"
        | otherwise = ""
      raw = T.concatMap step translit
      collapsed = T.concat (map squash (T.group raw))
      squash g = if T.head g == '-' then "-" else g
      trimmed = T.dropWhileEnd (== '-') (T.dropWhile (== '-') collapsed)
      final = T.take 128 trimmed
   in if slugSafe final then final else "playlist"

------------------------------------------------------------------------------
-- Модели списка
------------------------------------------------------------------------------

-- | Строка списка подборок.
data PlaylistEntry = PlaylistEntry
  { peSlug :: Text
    -- ^ Имя файла без расширения.
  , peTitle :: Text
  , peDescription :: Text
    -- ^ Пустая строка, если описания нет.
  , pePublic :: Bool
  , peLimit :: Maybe Integer
  , peSort :: Text
    -- ^ Краткое текстовое описание сортировки.
  , peSummary :: Text
    -- ^ Краткое текстовое дерево условий.
  , peStatus :: Text
    -- ^ @"managed"@, @"external"@, @"draft"@ или @"broken"@.
  , peManaged :: Bool
  , peExternal :: Bool
  , peStale :: Bool
    -- ^ 'True', если @.nsp@ существует без пары @.mix@.
  , peDraft :: Bool
    -- ^ 'True', если @.mix@ существует без опубликованного @.nsp@.
  , peMixFile :: Maybe String
  , peNspFile :: Maybe String
  , peModified :: Maybe UTCTime
  , peError :: Maybe Text
    -- ^ Текст ошибки разбора для 'peStatus' = @"broken"@.
  }
  deriving (Eq, Show)

instance ToJSON PlaylistEntry where
  toJSON p =
    object
      [ "slug" .= peSlug p
      , "name" .= peTitle p
      , "description" .= peDescription p
      , "public" .= pePublic p
      , "limit" .= peLimit p
      , "sort" .= peSort p
      , "summary" .= peSummary p
      , "status" .= peStatus p
      , "managed" .= peManaged p
      , "external" .= peExternal p
      , "stale" .= peStale p
      , "draft" .= peDraft p
      , "mixFile" .= peMixFile p
      , "nspFile" .= peNspFile p
      , "modified" .= peModified p
      , "error" .= peError p
      ]

-- | Детальная карточка подборки.
data PlaylistDetail = PlaylistDetail
  { pdEntry :: PlaylistEntry
  , pdDto :: Maybe PlaylistDto
    -- ^ 'Nothing', если файлы не удалось разобрать.
  , pdRaw :: Bool
    -- ^ 'True' для external: дерево содержит неизвестные узлы и
    -- редактирование запрещено.
  , pdMix :: Text
    -- ^ Предпросмотр @.mix@: исходный текст файла либо канонический
    -- рендер из DTO. Пусто, если ни то ни другое невозможно.
  , pdNsp :: Maybe Text
    -- ^ Исходный текст опубликованного @.nsp@.
  }
  deriving (Eq, Show)

instance ToJSON PlaylistDetail where
  toJSON d =
    object
      [ "entry" .= pdEntry d
      , "playlist" .= pdDto d
      , "external" .= pdRaw d
      , "editable" .= not (pdRaw d)
      , "mix" .= pdMix d
      , "nsp" .= pdNsp d
      ]

------------------------------------------------------------------------------
-- Корзина
------------------------------------------------------------------------------

-- | Запись корзины.
data TrashEntry = TrashEntry
  { teId :: Text
    -- ^ Имя каталога в корзине (@YYYYMMDDTHHMMSS-slug@).
  , teSlug :: Text
  , teDeletedAt :: Text
  , teMixFile :: Maybe String
  , teNspFile :: Maybe String
  , teDir :: FilePath
  }
  deriving (Eq, Show)

instance ToJSON TrashEntry where
  toJSON t =
    object
      [ "id" .= teId t
      , "slug" .= teSlug t
      , "deletedAt" .= teDeletedAt t
      , "mixFile" .= teMixFile t
      , "nspFile" .= teNspFile t
      ]

------------------------------------------------------------------------------
-- Сводка дерева и сортировки
------------------------------------------------------------------------------

-- | Есть ли в DTO неизвестные редактору узлы (raw). Определяет режим
-- external — такое дерево не выражимо в DSL и только читается.
containsRawDto :: PlaylistDto -> Bool
containsRawDto dto =
  maybe False isRawSort (pdSort dto) || any isRawItem (gdItems (pdRoot dto))
  where
    isRawSort = \case
      SortRawDto _ -> True
      _ -> False
    isRawItem = \case
      ItemRaw _ -> True
      ItemGroup g -> any isRawItem (gdItems g)
      ItemCond _ -> False

-- | Русское название поля для сводки (или идентификатор, если не найдено).
fieldTitle :: Text -> Text
fieldTitle fid = fromMaybe fid (listToMaybe [fsTitle f | f <- fieldSchemas, fsId f == fid])

-- | Русское название оператора для сводки.
opTitle :: Text -> Text
opTitle oid = fromMaybe oid (listToMaybe [osName o | o <- operatorSchemas, osId o == oid])

-- | Значение условия → человекочитаемый текст.
valueText :: Maybe Value -> Text
valueText = \case
  Nothing -> ""
  Just (String t) -> "«" <> t <> "»"
  Just (Bool True) -> "да"
  Just (Bool False) -> "нет"
  Just (Number n) -> tshow n
  Just (Array arr) -> case foldr (:) [] arr of
    [x, y] -> valueText (Just x) <> " … " <> valueText (Just y)
    _ -> "…"
  Just _ -> "…"

-- | Строка сортировки: @"случайно"@ или список полей с направлением.
sortTextOf :: Maybe SortDto -> Text
sortTextOf = \case
  Nothing -> "по умолчанию"
  Just SortRandomDto -> "случайно"
  Just (SortFieldsDto items) ->
    T.intercalate ", " [fieldTitle (siField s) <> " " <> dirText (siDir s) | s <- items]
  Just (SortRawDto t) -> t
  where
    dirText d = if d == "desc" then "убыв" else "возр"

-- | Краткое текстовое дерево условий для списка.
summarizeDto :: PlaylistDto -> Text
summarizeDto dto = summarizeGroup 0 (pdRoot dto)

summarizeGroup :: Int -> GroupDto -> Text
summarizeGroup depth g =
  let kind = case gdKind g of
        "all" -> "ВСЕ"
        "any" -> "ЛЮБОЕ"
        other -> T.toUpper other
      sep = if gdKind g == "any" then " ∨ " else " ∧ "
      items = gdItems g
   in if depth >  3
        then kind <> " …"
        else case items of
          [] -> kind <> " (пусто)"
          _ -> kind <> ": " <> T.intercalate sep (map (summarizeItem depth) items)

summarizeItem :: Int -> ItemDto -> Text
summarizeItem depth = \case
  ItemCond c -> summarizeCond c
  ItemGroup g -> "(" <> summarizeGroup (depth + 1) g <> ")"
  ItemRaw _ -> "[внешний узел]"

summarizeCond :: CondDto -> Text
summarizeCond c =
  let t = valueText (cdValue c)
      op = opTitle (cdOp c)
   in fieldTitle (cdField c)
        <> " "
        <> ( if T.isInfixOf "N" op
               then T.replace "N" t op
               else if T.null t then op else op <> " " <> t
           )

------------------------------------------------------------------------------
-- Работа с файлами
------------------------------------------------------------------------------

-- | Создать каталоги хранилища (ишутся рекурсивно).
ensureStoreDirs :: StoreConfig -> IO (Either StoreError ())
ensureStoreDirs cfg = do
  a <- ensureDir (scRulesDir cfg)
  case a of
    Left e -> pure (Left e)
    Right () -> do
      b <- ensureDir (scPlaylistsDir cfg)
      case b of
        Left e -> pure (Left e)
        Right () -> ensureDir (scTrashDir cfg)

ensureDir :: FilePath -> IO (Either StoreError ())
ensureDir dir = do
  r <- try (createDirectoryIfMissing True dir) :: IO (Either IOException ())
  pure $ case r of
    Left _ -> Left (StoreIo ("Не удалось создать каталог: " <> T.pack dir))
    Right () -> Right ()

-- | Прочитать файл как UTF-8.
readTextFile :: FilePath -> IO (Either StoreError Text)
readTextFile path = do
  r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case r of
    Left _ -> Left (StoreIo ("Не удалось прочитать файл: " <> T.pack path))
    Right bs -> case decodeUtf8' bs of
      Left _ -> Left (StoreIo ("Файл не в кодировке UTF-8: " <> T.pack path))
      Right t -> Right t

fileMtime :: FilePath -> IO (Maybe UTCTime)
fileMtime path = do
  r <- try (getModificationTime path) :: IO (Either IOException UTCTime)
  pure (either (const Nothing) Just r)

newestMTime :: [FilePath] -> IO (Maybe UTCTime)
newestMTime paths = do
  ts <- mapM fileMtime paths
  pure (foldr max Nothing ts)

removeQuiet :: FilePath -> IO ()
removeQuiet p = void (try (removeFile p) :: IO (Either IOException ()))

renameQuiet :: FilePath -> FilePath -> IO Bool
renameQuiet src dst = do
  r <- try (renameFile src dst) :: IO (Either IOException ())
  pure (either (const False) (const True) r)

-- | Принадлежит ли @child@ каталогу @parent@ (после канонизации).
childOf :: FilePath -> FilePath -> Bool
childOf parent child =
  let ps = splitDirectories parent
      cs = splitDirectories child
   in length cs > length ps && take (length ps) cs == ps

-- | Проверка целевого файла перед записью: каталог существует, файл не
-- является символической ссылкой и не выходит за его пределы.
checkTarget :: FilePath -> IO (Either StoreError ())
checkTarget path = do
  let parent = takeDirectory path
      name = takeFileName path
  parentExists <- doesDirectoryExist parent
  isDir <- doesDirectoryExist path
  fileExists <- doesFileExist path
  if isDir
    then pure (Left (StoreConflict (T.pack name)))
    else
      if not parentExists || not fileExists
        then pure (Right ())
        else do
          linkR <- try (pathIsSymbolicLink path) :: IO (Either IOException Bool)
          case linkR of
            Left _ ->
              pure (Left (StoreIo ("Не удалось проверить файл: " <> T.pack path)))
            Right True -> pure (Left (StoreUnsafePath path))
            Right False -> do
              cFile <- try (canonicalizePath path) :: IO (Either IOException FilePath)
              cDir <- try (canonicalizePath parent) :: IO (Either IOException FilePath)
              pure $ case (cFile, cDir) of
                (Right f, Right d)
                  | childOf d f -> Right ()
                  | otherwise -> Left (StoreUnsafePath path)
                _ ->
                  Left (StoreIo ("Не удалось канонизировать путь: " <> T.pack path))

-- | Замещение набора файлов: запись во временные файлы рядом с целевыми,
-- затем rename с резервной копией и откатом при любой ошибке.
replaceFiles :: [(FilePath, BS.ByteString)] -> IO (Either StoreError ())
replaceFiles items = do
  sr <- writeStages items
  case sr of
    Left e -> removeStages >> pure (Left e)
    Right () -> loop items [] []
  where
    stageOf t = takeDirectory t </> (('.' : takeFileName t) ++ ".tmp")
    backupOf t = takeDirectory t </> (('.': takeFileName t) ++ ".bak")

    writeStages [] = pure (Right ())
    writeStages ((t, bs) : xs) = do
      r <- try (BS.writeFile (stageOf t) bs) :: IO (Either IOException ())
      case r of
        Left _ ->
          pure (Left (StoreIo ("Не удалось записать временный файл: " <> T.pack (stageOf t))))
        Right () -> writeStages xs

    removeStages = mapM_ (\(t, _) -> removeQuiet (stageOf t)) items

    rollback bks noBak = do
      mapM_ removeQuiet noBak
      forM_ bks $ \(t, b) -> do
        removeQuiet t
        ex <- doesFileExist b
        when ex $ void (renameQuiet b t)
      removeStages

    loop [] bks _noBak = do
      mapM_ (removeQuiet . snd) bks
      removeStages
      pure (Right ())
    loop ((t, _) : xs) bks noBak = do
      ex <- doesFileExist t
      bakM <-
        if ex
          then do
            ok <- renameQuiet t (backupOf t)
            pure $
              if ok
                then Right (Just (backupOf t))
                else Left (StoreIo ("Не удалось создать резервную копию: " <> T.pack t))
          else pure (Right Nothing)
      case bakM of
        Left e -> rollback bks noBak >> pure (Left e)
        Right mb -> do
          ok <- renameQuiet (stageOf t) t
          if not ok
            then do
              rollback (maybe bks (\b -> (t, b) : bks) mb) noBak
              pure (Left (StoreIo ("Не удалось заменить файл: " <> T.pack t)))
            else case mb of
              Just b -> loop xs ((t, b) : bks) noBak
              Nothing -> loop xs bks (t : noBak)

------------------------------------------------------------------------------
-- Загрузка записей
------------------------------------------------------------------------------

-- | Внутренний результат чтения файлов подборки.
data Loaded = Loaded
  { lEntry :: PlaylistEntry
  , lDto :: Maybe PlaylistDto
  , lMix :: Maybe Text
  , lNsp :: Maybe Text
  }

mixPathOf :: StoreConfig -> Text -> FilePath
mixPathOf cfg slug = scRulesDir cfg </> (T.unpack slug ++ ".mix")

nspPathOf :: StoreConfig -> Text -> FilePath
nspPathOf cfg slug = scPlaylistsDir cfg </> (T.unpack slug ++ ".nsp")

-- | Разбор @.nsp@ в DTO.
decodeNspText :: Text -> Either Text PlaylistDto
decodeNspText t = case eitherDecode (LBS.fromStrict (encodeUtf8 t)) :: Either String Value of
  Left e -> Left ("Некорректный JSON в .nsp: " <> T.pack e)
  Right v -> first ("Не удалось разобрать .nsp: " <>) (nspToDto v)

emptyDto :: Text -> PlaylistDto
emptyDto slug = PlaylistDto slug Nothing False (GroupDto "all" []) Nothing Nothing

-- | Прочитать подборку по slug: @.mix@ предпочтителен как источник DSL,
-- @.nsp@ с неизвестными узлами делает подборку external.
loadEntry :: StoreConfig -> Text -> IO (Maybe Loaded)
loadEntry cfg slug = do
  let mixP = mixPathOf cfg slug
      nspP = nspPathOf cfg slug
  hasMix <- doesFileExist mixP
  hasNsp <- doesFileExist nspP
  if not hasMix && not hasNsp
    then pure Nothing
    else do
      modT <- newestMTime [p | (True, p) <- [(hasMix, mixP), (hasNsp, nspP)]]
      let base =
            PlaylistEntry
              { peSlug = slug
              , peTitle = slug
              , peDescription = ""
              , pePublic = False
              , peLimit = Nothing
              , peSort = ""
              , peSummary = ""
              , peStatus = "broken"
              , peManaged = False
              , peExternal = False
              , peStale = hasNsp && not hasMix
              , peDraft = hasMix && not hasNsp
              , peMixFile = if hasMix then Just (T.unpack slug ++ ".mix") else Nothing
              , peNspFile = if hasNsp then Just (T.unpack slug ++ ".nsp") else Nothing
              , peModified = modT
              , peError = Nothing
              }
      if not (slugReadable slug)
        then
          pure $
            Just
              Loaded
                { lEntry =
                    base
                      { peError =
                          Just "Имя файла не является допустимым — подборка недоступна в редакторе."
                      }
                , lDto = Nothing
                , lMix = Nothing
                , lNsp = Nothing
                }
        else do
          mixR <- if hasMix then Just <$> readTextFile mixP else pure Nothing
          nspR <- if hasNsp then Just <$> readTextFile nspP else pure Nothing
          let ioErrs = [e | Just (Left e) <- [mixR, nspR]]
          if not (null ioErrs)
            then
              pure $
                Just
                  Loaded
                    { lEntry = base {peError = Just (T.intercalate "; " (map storeErrorMessage ioErrs))}
                    , lDto = Nothing
                    , lMix = Nothing
                    , lNsp = Nothing
                    }
            else do
              let mixTxt = either (const Nothing) Just =<< mixR
                  nspTxt = either (const Nothing) Just =<< nspR
                  nE = maybe (Right Nothing) decodeNspDto nspTxt
                  mE = maybe (Right Nothing) mixDto mixTxt
                  nD = either (const Nothing) id nE
                  mD = either (const Nothing) id mE
                  rawN = maybe False containsRawDto nD
                  errT = case (nE, mE) of
                    (Left e, _) -> Just e
                    (_, Left e) -> Just e
                    _ -> Nothing
                  chosen =
                    asum
                      [ if rawN then fmap (\n -> (n, "external")) nD else Nothing
                      , fmap (\m -> (m, "managed")) mD
                      , fmap (\n -> (n, "managed")) nD
                      ]
                  finalEntry dto status =
                    base
                      { peTitle = pdName dto
                      , peDescription = fromMaybe "" (pdDescription dto)
                      , pePublic = pdPublic dto
                      , peLimit = pdLimit dto
                      , peSort = sortTextOf (pdSort dto)
                      , peSummary = summarizeDto dto
                      , peStatus = status
                      , peManaged = status == "managed"
                      , peExternal = status == "external"
                      , peError = Nothing
                      }
              pure $
                Just $
                  case (errT, chosen) of
                    (Just e, _) ->
                      let dto = fromMaybe (emptyDto slug) (nD <|> mD)
                       in Loaded
                            { lEntry =
                                base
                                  { peTitle = pdName dto
                                  , peDescription = fromMaybe "" (pdDescription dto)
                                  , peError = Just e
                                  }
                            , lDto = Nothing
                            , lMix = mixTxt
                            , lNsp = nspTxt
                            }
                    (Nothing, Just (dto, status)) ->
                      Loaded
                        { lEntry = finalEntry dto status
                        , lDto = Just dto
                        , lMix = mixTxt
                        , lNsp = nspTxt
                        }
                    (Nothing, Nothing) ->
                      Loaded
                        { lEntry = base {peError = Just "Не удалось разобрать файлы подборки."}
                        , lDto = Nothing
                        , lMix = mixTxt
                        , lNsp = nspTxt
                        }
  where
    decodeNspDto t = case decodeNspText t of
      Left e -> Left e
      Right d -> Right (Just d)
    mixDto t = case dtoFromMix t of
      Left es -> Left (apiErrorsText es)
      Right d -> Right (Just d)

------------------------------------------------------------------------------
-- Список и чтение
------------------------------------------------------------------------------

-- | Список подборок из обоих каталогов, отсортированный по времени
-- изменения (новые сверху), затем по названию.
listPlaylists :: StoreConfig -> IO (Either StoreError [PlaylistEntry])
listPlaylists cfg = do
  ed <- ensureStoreDirs cfg
  case ed of
    Left e -> pure (Left e)
    Right () -> do
      mixes <- listByExt (scRulesDir cfg) ".mix"
      nsps <- listByExt (scPlaylistsDir cfg) ".nsp"
      let slugs = nub (map baseSlug mixes ++ map baseSlug nsps)
      loaded <- mapM (loadEntry cfg) (filter (not . T.null) slugs)
      pure (Right (sortOn entryKey [lEntry l | Just l <- loaded]))
  where
    baseSlug p = T.pack (takeBaseName p)
    entryKey e = (Down (peModified e), T.toLower (peTitle e))

listByExt :: FilePath -> String -> IO [FilePath]
listByExt dir ext = do
  names <- listDirectory dir
  let candidates = [dir </> n | n <- names, takeExtension n == ext]
  filterM doesFileExist candidates

-- | Прочитать подборку целиком (DTO, предпросмотр @.mix@, текст @.nsp@).
readPlaylist :: StoreConfig -> Text -> IO (Either StoreError PlaylistDetail)
readPlaylist cfg slug
  | not (slugReadable slug) = pure (Left (StoreUnsafeSlug slug))
  | otherwise = do
      loaded <- loadEntry cfg slug
      case loaded of
        Nothing -> pure (Left (StoreNotFound slug))
        Just l -> do
          let entry = lEntry l
              rendered = case (lDto l, peExternal entry) of
                (Just d, False) -> case dtoToParsed d of
                  Right p -> renderParsedFile p
                  Left _ -> ""
                _ -> ""
          pure $
            Right
              PlaylistDetail
                { pdEntry = entry
                , pdDto = lDto l
                , pdRaw = peExternal entry
                , pdMix = fromMaybe rendered (lMix l)
                , pdNsp = lNsp l
                }

------------------------------------------------------------------------------
-- Публикация
------------------------------------------------------------------------------

-- | Опубликовать (или перезаписать) подборку: DTO → валидация ядра →
-- канонический @.mix@ и @.nsp@ → атомарное замещение обоих файлов.
--
-- @overwrite = False@ приводит к 'StoreConflict', если хотя бы один из
-- файлов уже существует.
publishPlaylist ::
  StoreConfig ->
  Text ->
  Bool ->
  PlaylistDto ->
  IO (Either StoreError PlaylistDetail)
publishPlaylist cfg slug overwrite dto = do
  ed <- ensureStoreDirs cfg
  case ed of
    Left e -> pure (Left e)
    Right ()
      | not (slugSafe slug) -> pure (Left (StoreUnsafeSlug slug))
      | otherwise -> do
          let mixT = mixPathOf cfg slug
              nspT = nspPathOf cfg slug
          c1 <- checkTarget mixT
          case c1 of
            Left e -> pure (Left e)
            Right () -> do
              c2 <- checkTarget nspT
              case c2 of
                Left e -> pure (Left e)
                Right () -> do
                  exMix <- doesFileExist mixT
                  exNsp <- doesFileExist nspT
                  if (exMix || exNsp) && not overwrite
                    then pure (Left (StoreConflict slug))
                    else case compilePlaylistDto dto of
                      Left errs -> pure (Left (StoreInvalid slug errs))
                      Right compiled -> do
                        let mixBs = encodeUtf8 (cmpMix compiled)
                            nspBs = strictNsp (cmpNsp compiled)
                        r <-
                          replaceFiles [(mixT, mixBs), (nspT, nspBs)]
                        case r of
                          Left e -> pure (Left e)
                          Right () -> readPlaylist cfg slug
  where
    strictNsp = LBS.toStrict

------------------------------------------------------------------------------
-- Удаление в корзину
------------------------------------------------------------------------------

-- | Перенести @.mix@ и @.nsp@ в корзину и записать @meta.txt@.
--
-- Прямого удаления нет: при сбое уже перенесённые файлы возвращаются
-- на место; если возврат не удался — 'StorePartial'.
deletePlaylistFiles ::
  StoreConfig -> Text -> IO (Either StoreError Text)
deletePlaylistFiles cfg slug = do
  if not (slugReadable slug)
    then pure (Left (StoreUnsafeSlug slug))
    else do
      ed <- ensureStoreDirs cfg
      case ed of
        Left e -> pure (Left e)
        Right () -> do
          let mixT = mixPathOf cfg slug
              nspT = nspPathOf cfg slug
          hasMix <- doesFileExist mixT
          hasNsp <- doesFileExist nspT
          if not hasMix && not hasNsp
            then pure (Left (StoreNotFound slug))
            else do
              now <- getCurrentTime
              let ts = formatTime defaultTimeLocale "%Y%m%dT%H%M%S" now
                  dirName0 = ts <> "-" <> T.unpack slug
              dirName <- uniqueDirName (scTrashDir cfg) dirName0
              let dir = scTrashDir cfg </> dirName
                  moves =
                    [(mixT, dir </> takeFileName mixT, hasMix)]
                      ++ [(nspT, dir </> takeFileName nspT, hasNsp)]
              createR <- try (createDirectory dir) :: IO (Either IOException ())
              case createR of
                Left _ ->
                  pure (Left (StoreIo ("Не удалось создать каталог корзины: " <> T.pack dir)))
                Right () -> do
                  results <- forM moves $ \(src, dst, needed) ->
                    if needed
                      then do
                        ok <- renameQuiet src dst
                        pure (if ok then Right (Just dst, src) else Left src)
                      else pure (Right (Nothing, src))
                  let failed = [p | Left p <- results]
                      done = [(d, s) | Right (Just d, s) <- results]
                  if null failed
                    then do
                      ok <- writeMetaFile dir slug now
                      if ok
                        then pure (Right (T.pack dirName))
                        else do
                          rolled <- rollbackMoves done
                          removeDirQuiet dir
                          pure $
                            if rolled
                              then Left (StoreIo "Не удалось записать служебный файл корзины.")
                              else
                                Left
                                  ( StorePartial
                                      "Частичное удаление: служебный файл корзины не записан, и часть файлов вернуть не удалось."
                                  )
                    else do
                      rolled <- rollbackMoves done
                      removeDirQuiet dir
                      pure $
                        if rolled
                          then
                            Left
                              ( StoreIo
                                  ( "Не удалось перенести в корзину: "
                                      <> T.intercalate ", " (map T.pack failed)
                                  )
                              )
                          else
                            Left
                              ( StorePartial
                                  ( "Частичное удаление: не перенесено "
                                      <> T.intercalate ", " (map T.pack failed)
                                      <> "; вернуть на место не удалось — "
                                      <> T.intercalate ", " (map (T.pack . snd) done)
                                  )
                              )

rollbackMoves :: [(FilePath, FilePath)] -> IO Bool
rollbackMoves = go True
  where
    go acc [] = pure acc
    go acc ((dst, src) : xs) = do
      ok <- renameQuiet dst src
      go (acc && ok) xs

writeMetaFile :: FilePath -> Text -> UTCTime -> IO Bool
writeMetaFile dir slug at = do
  let meta =
        "slug="
          <> slug
          <> "\ndeletedAt="
          <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" at)
          <> "\n"
  r <- try (BS.writeFile (dir </> "meta.txt") (encodeUtf8 meta)) :: IO (Either IOException ())
  pure (either (const False) (const True) r)

uniqueDirName :: FilePath -> String -> IO String
uniqueDirName base name0 = go (0 :: Int)
  where
    go :: Int -> IO String
    go i = do
      let n = if i == 0 then name0 else name0 <> "-" <> show i
      ex <- doesDirectoryExist (base </> n)
      if ex then go (i + 1) else pure n

removeDirQuiet :: FilePath -> IO ()
removeDirQuiet dir = do
  ex <- doesDirectoryExist dir
  when ex $ void (try (removeDirectory dir) :: IO (Either IOException ()))

------------------------------------------------------------------------------
-- Корзина: список, восстановление, очистка
------------------------------------------------------------------------------

-- | Содержимое корзины (новые записи сверху).
listTrash :: StoreConfig -> IO (Either StoreError [TrashEntry])
listTrash cfg = do
  ed <- ensureStoreDirs cfg
  case ed of
    Left e -> pure (Left e)
    Right () -> do
      names <- listDirectory (scTrashDir cfg)
      entries <- forM names $ \n -> do
        let dir = scTrashDir cfg </> n
        isDir <- doesDirectoryExist dir
        if not isDir
          then pure Nothing
          else do
            meta <- readMeta (dir </> "meta.txt")
            let (mSlug, mAt) = maybe (Nothing, Nothing) parseMeta meta
                slug = fromMaybe (fallbackSlug n) mSlug
                at = fromMaybe (fallbackStamp n) mAt
                mixName = T.unpack slug ++ ".mix"
                nspName = T.unpack slug ++ ".nsp"
            hasMix <- doesFileExist (dir </> mixName)
            hasNsp <- doesFileExist (dir </> nspName)
            pure $
              Just
                TrashEntry
                  { teId = T.pack n
                  , teSlug = slug
                  , teDeletedAt = at
                  , teMixFile = if hasMix then Just mixName else Nothing
                  , teNspFile = if hasNsp then Just nspName else Nothing
                  , teDir = dir
                  }
      pure (Right (sortOn (Down . teDeletedAt) [e | Just e <- entries]))
  where
    readMeta p = do
      r <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Left _ -> Nothing
        Right bs -> either (const Nothing) Just (decodeUtf8' bs)

-- | Разбор @meta.txt@ корзины.
parseMeta :: Text -> (Maybe Text, Maybe Text)
parseMeta txt = (lookup "slug" kvs, lookup "deletedAt" kvs)
  where
    kvs =
      [ (k, v)
      | line <- T.lines txt
      , let (k, rest) = T.breakOn "=" line
      , not (T.null k)
      , let v = T.drop 1 rest
      ]

-- | Slug из имени каталога корзины (@YYYYMMDDTHHMMSS-slug@).
fallbackSlug :: String -> Text
fallbackSlug n = T.drop 16 (T.pack n)

-- | Метка времени из имени каталога корзины.
fallbackStamp :: String -> Text
fallbackStamp n = T.pack (take 15 n)

-- | Восстановить запись корзины на место.
restoreTrash :: StoreConfig -> Text -> IO (Either StoreError ())
restoreTrash cfg tid = do
  if not (slugReadable tid)
    then pure (Left (StoreUnsafeSlug tid))
    else do
      ed <- ensureStoreDirs cfg
      case ed of
        Left e -> pure (Left e)
        Right () -> do
          let dir = scTrashDir cfg </> T.unpack tid
          isDir <- doesDirectoryExist dir
          if not isDir
            then pure (Left (StoreNotFound tid))
            else do
              metaR <- try (BS.readFile (dir </> "meta.txt")) :: IO (Either IOException BS.ByteString)
              let metaTxt = either (const Nothing) (either (const Nothing) Just . decodeUtf8') metaR
                  slug = fromMaybe (fallbackSlug (T.unpack tid)) (fst . parseMeta =<< metaTxt)
              if not (slugReadable slug)
                then pure (Left (StoreUnsafeSlug slug))
                else do
                  let mixT = mixPathOf cfg slug
                      nspT = nspPathOf cfg slug
                      moves =
                        [(dir </> takeFileName mixT, mixT, T.unpack slug ++ ".mix")]
                          ++ [(dir </> takeFileName nspT, nspT, T.unpack slug ++ ".nsp")]
                  targets <- forM moves $ \(_, dst, name) -> do
                    ex <- doesFileExist dst
                    pure (ex, name)
                  let clash = [n | (True, n) <- targets]
                  if not (null clash)
                    then
                      pure $
                        Left
                          ( StoreConflict
                              (T.intercalate ", " (map T.pack clash) <> " — уже существуют")
                          )
                    else do
                      present <- forM moves $ \(src, dst, name) -> do
                        ex <- doesFileExist src
                        pure [(src, dst, name) | ex]
                      results <- forM (concat present) $ \(src, dst, name) -> do
                        ok <- renameQuiet src dst
                        pure (dst, src, name, ok)
                      let failed = [n | (_, _, n, False) <- results]
                          done = [(d, s) | (d, s, _, True) <- results]
                          names = T.intercalate ", " (map T.pack failed)
                      if null failed
                        then do
                          void (try (removeDirectoryRecursive dir) :: IO (Either IOException ()))
                          pure (Right ())
                        else do
                          rolled <- rollbackMoves done
                          pure $
                            if rolled
                              then Left (StoreIo ("Не удалось восстановить: " <> names))
                              else
                                Left
                                  ( StorePartial
                                      ("Частичное восстановление: вернуть на место не удалось — " <> names)
                                  )

-- | Удалить запись корзины безвозвратно.
purgeTrash :: StoreConfig -> Text -> IO (Either StoreError ())
purgeTrash cfg tid = do
  if not (slugReadable tid)
    then pure (Left (StoreUnsafeSlug tid))
    else do
      let dir = scTrashDir cfg </> T.unpack tid
      isDir <- doesDirectoryExist dir
      if not isDir
        then pure (Left (StoreNotFound tid))
        else do
          r <- try (removeDirectoryRecursive dir) :: IO (Either IOException ())
          pure $ case r of
            Left _ -> Left (StoreIo ("Не удалось удалить запись корзины: " <> tid))
            Right () -> Right ()

------------------------------------------------------------------------------
-- Мелочи
------------------------------------------------------------------------------

-- | Безопасная склейка текста (для ошибок без @Show@).
tshow :: Show a => a -> Text
tshow = T.pack . show
