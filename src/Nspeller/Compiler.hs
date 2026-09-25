{-# LANGUAGE OverloadedStrings #-}

-- | Конвейер компиляции: 'Text' → разобранный AST → валидированный
-- AST → модель Navidrome → JSON.
--
-- Модуль объединяет этапы, но не содержит собственной логики разбора
-- или проверок: парсер не генерирует JSON, валидация не знает о JSON.
module Nspeller.Compiler
  ( compileText
  , compileTextWithWarnings
  ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Nspeller.Ast (CompileError)
import Nspeller.Navidrome (NspPlaylist, toNsp)
import Nspeller.Parser (parsePlaylist)
import Nspeller.Validation (validatePlaylistWithWarnings)

-- | Полный конвейер до модели Navidrome.
--
-- Синтаксические ошибки превращаются в список из одного элемента.
-- Предупреждения валидации отбрасываются — см.
-- 'compileTextWithWarnings'.
compileText :: FilePath -> Text -> Either [CompileError] NspPlaylist
compileText fp src = fmap fst (compileTextWithWarnings fp src)

-- | Как 'compileText', но возвращает также предупреждения
-- (избыточные условия, покрытие домена и т. п.). Предупреждения
-- возвращаются только при успешной компиляции и на результат не
-- влияют.
compileTextWithWarnings ::
  FilePath ->
  Text ->
  Either [CompileError] (NspPlaylist, [CompileError])
compileTextWithWarnings fp src = do
  parsed <- first pure (parsePlaylist fp src)
  (valid, warns) <- validatePlaylistWithWarnings fp src parsed
  pure (toNsp valid, warns)
