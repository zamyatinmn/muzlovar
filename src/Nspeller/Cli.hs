{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Интерфейс командной строки @nspeller@.
--
-- Команды:
--
-- * @check FILE@ — проверить файл, ничего не записывая;
-- * @build FILE [--output PATH]@ — скомпилировать один файл;
-- * @build-all DIR [--output DIR]@ — скомпилировать каталог.
--
-- @build-all@ сначала проверяет все исходники и записывает файлы
-- только если все они валидны; запись каждого файла атомарна
-- (временный файл в том же каталоге + rename).
module Nspeller.Cli
  ( runCli
  , checkFile
  , buildFile
  , buildAll
  ) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (filterM, forM, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nspeller.Ast (CompileError (..), fileError, renderCompileError)
import Nspeller.Compiler (compileText)
import Nspeller.Navidrome (encodeNsp)
import Options.Applicative
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  , renameFile
  )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (splitExtension, takeDirectory, takeFileName, (</>), (<.>))
import System.IO (hPutStr, hSetEncoding, stderr, stdout, utf8)

------------------------------------------------------------------------------
-- Разбор аргументов
------------------------------------------------------------------------------

data Command
  = CheckCmd FilePath
  | BuildCmd FilePath (Maybe FilePath)
  | BuildAllCmd FilePath (Maybe FilePath)

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "check"
        (info (CheckCmd <$> fileArg) (progDesc "Проверить .mix-файл без записи"))
        <> command
          "build"
          (info (BuildCmd <$> fileArg <*> outputArg) (progDesc "Скомпилировать один .mix-файл в .nsp"))
        <> command
          "build-all"
          (info (BuildAllCmd <$> dirArg <*> outputArg) (progDesc "Скомпилировать все .mix-файлы каталога"))
    )

fileArg :: Parser FilePath
fileArg = strArgument (metavar "FILE" <> help "Путь к файлу .mix")

dirArg :: Parser FilePath
dirArg =
  strArgument
    ( metavar "DIR"
        <> help "Каталог с файлами .mix (обход только верхнего уровня)"
    )

outputArg :: Parser (Maybe FilePath)
outputArg =
  optional $
    strOption
      ( long "output"
          <> metavar "PATH"
          <> help "Куда писать результат (для build-all — каталог; по умолчанию — рядом с источником)"
      )

-- | Точка входа: разбирает аргументы, выполняет команду и
-- завершает процесс с подходящим кодом возврата.
runCli :: IO ()
runCli = do
  -- Русские сообщения и имена подборок печатаются в UTF-8
  -- независимо от локали консоли.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  cmd <-
    execParser $
      info
        (commandParser <**> helper)
        ( fullDesc
            <> header "nspeller — компилятор умных подборок Navidrome (.mix → .nsp)"
            <> progDesc "Правила подборок на русском языке компилируются в JSON, понятный Navidrome."
        )
  ok <- case cmd of
    CheckCmd f -> checkFile f
    BuildCmd f out -> buildFile f out
    BuildAllCmd inDir outDir -> buildAll inDir (fromMaybe inDir outDir)
  if ok then exitSuccess else exitFailure

------------------------------------------------------------------------------
-- Чтение и вывод
------------------------------------------------------------------------------

-- | Печатает ошибки в stderr: по блоку на каждую, блоки разделяются
-- пустой строкой.
putErrors :: [CompileError] -> IO ()
putErrors errs =
  hPutStr stderr . T.unpack . (`T.snoc` '\n') . T.intercalate "\n\n" $
    case errs of
      [] -> ["Внутренняя ошибка: список ошибок пуст."]
      xs -> map renderCompileError xs

-- | Читает файл строго как UTF-8.
readMixFile :: FilePath -> IO (Either [CompileError] Text)
readMixFile fp = do
  res <- try (BS.readFile fp)
  pure $ case res of
    Left (e :: IOException) ->
      Left [fileError fp ("не удалось прочитать файл: " <> T.pack (displayException e))]
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _ -> Left [fileError fp "файл не является корректным UTF-8"]
      Right txt -> Right txt

------------------------------------------------------------------------------
-- check
------------------------------------------------------------------------------

-- | Проверяет один файл. 'True' — успех.
checkFile :: FilePath -> IO Bool
checkFile fp = do
  readRes <- readMixFile fp
  case readRes of
    Left errs -> putErrors errs >> pure False
    Right src -> case compileText fp src of
      Left errs -> putErrors errs >> pure False
      Right _ -> putStrLn ("OK: " <> fp) >> pure True

------------------------------------------------------------------------------
-- build
------------------------------------------------------------------------------

-- | Путь @.nsp@ для источника: @foo.mix@ → @foo.nsp@.
nspPathFor :: FilePath -> FilePath
nspPathFor p = fst (splitExtension p) <.> "nsp"

-- | Атомарная запись: временный файл в том же каталоге + rename.
-- Родительские каталоги создаются при необходимости.
writeAtomic :: FilePath -> LBS.ByteString -> IO (Either CompileError ())
writeAtomic target bytes = do
  let dir = takeDirectory target
      tmp = target <> ".tmp"
  res <- try $ do
    createDirectoryIfMissing True dir
    LBS.writeFile tmp bytes
    renameFile tmp target
  pure $ case res of
    Left (e :: IOException) ->
      Left (fileError target ("не удалось записать файл: " <> T.pack (displayException e)))
    Right () -> Right ()

-- | Скомпилировать один файл. 'True' — успех.
buildFile :: FilePath -> Maybe FilePath -> IO Bool
buildFile fp mOut = do
  readRes <- readMixFile fp
  case readRes of
    Left errs -> putErrors errs >> pure False
    Right src -> case compileText fp src of
      Left errs -> putErrors errs >> pure False
      Right nsp -> do
        let target = fromMaybe (nspPathFor fp) mOut
        writeRes <- writeAtomic target (encodeNsp nsp)
        case writeRes of
          Left err -> putErrors [err] >> pure False
          Right () -> putStrLn ("OK: " <> fp <> " -> " <> target) >> pure True

------------------------------------------------------------------------------
-- build-all
------------------------------------------------------------------------------

-- | Скомпилировать каталог: сначала проверяются все @.mix@-файлы
-- верхнего уровня; при любой ошибке не записывается ни один файл и
-- выводятся все ошибки. При успехе каждый @foo.mix@ атомарно
-- превращается в @foo.nsp@. Старые @.nsp@ без парных @.mix@
-- не удаляются.
buildAll :: FilePath -> FilePath -> IO Bool
buildAll inDir outDir = do
  listed <- try (listDirectory inDir)
  case listed of
    Left (e :: IOException) -> do
      putErrors [fileError inDir ("не удалось прочитать каталог: " <> T.pack (displayException e))]
      pure False
    Right entries -> do
      -- Имена из listDirectory разрешаются относительно inDir,
      -- а не текущего каталога процесса.
      names <- filterM (\name -> doesFileExist (inDir </> name)) (filter isMix (sort entries))
      if null names
        then do
          putStrLn ("Нет файлов .mix в " <> inDir)
          pure True
        else do
          compiled <- forM names $ \name -> do
            let srcPath = inDir </> name
                target = outDir </> nspPathFor (takeFileName name)
            readRes <- readMixFile srcPath
            pure $ case readRes of
              Left errs -> Left errs
              Right src -> case compileText srcPath src of
                Left errs -> Left errs
                Right nsp -> Right (target, encodeNsp nsp)
          case concat [errs | Left errs <- compiled] of
            errs@(_ : _) -> putErrors errs >> pure False
            [] -> do
              let outputs = [(target, bytes) | Right (target, bytes) <- compiled]
              results <- forM outputs $ \(target, bytes) -> do
                writeRes <- writeAtomic target bytes
                pure $ case writeRes of
                  Left err -> Left err
                  Right () -> Right target
              case [err | Left err <- results] of
                errs@(_ : _) -> putErrors errs >> pure False
                [] -> do
                  forM_ outputs $ \(target, _) -> putStrLn ("OK: " <> target)
                  pure True
  where
    isMix name = snd (splitExtension name) == ".mix"
