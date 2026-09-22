{-# LANGUAGE OverloadedStrings #-}

-- | Конвейер компиляции: 'Text' → разобранный AST → валидированный
-- AST → модель Navidrome → JSON.
--
-- Модуль объединяет этапы, но не содержит собственной логики разбора
-- или проверок: парсер не генерирует JSON, валидация не знает о JSON.
module Nspeller.Compiler
  ( compileText
  ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Nspeller.Ast (CompileError)
import Nspeller.Navidrome (NspPlaylist, toNsp)
import Nspeller.Parser (parsePlaylist)
import Nspeller.Validation (validatePlaylist)

-- | Полный конвейер до модели Navidrome.
--
-- Синтаксические ошибки превращаются в список из одного элемента.
compileText :: FilePath -> Text -> Either [CompileError] NspPlaylist
compileText fp src = do
  parsed <- first pure (parsePlaylist fp src)
  valid <- validatePlaylist fp src parsed
  pure (toNsp valid)
