{-# LANGUAGE TemplateHaskell #-}

-- | Статические ресурсы Muzlovar, вшитые в бинарник.
--
-- Никакого npm и сборщика фронтенда нет: CSS и JS лежат в
-- @muzlovar/static/@ и подключаются через 'Data.FileEmbed.embedFile'.
-- SortableJS 1.15.7 вендорен в репозитории, чтобы интерфейс работал
-- без доступа в интернет.
module Nspeller.Muzlovar.Assets
  ( muzlovarCss
  , muzlovarJs
  , sortableJs
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

-- | Стили тёмной темы.
muzlovarCss :: ByteString
muzlovarCss = $(embedFile "muzlovar/static/muzlovar.css")

-- | Логика редактора (vanilla JS).
muzlovarJs :: ByteString
muzlovarJs = $(embedFile "muzlovar/static/muzlovar.js")

-- | Vendored SortableJS 1.15.7.
sortableJs :: ByteString
sortableJs = $(embedFile "muzlovar/static/sortable.min.js")
