{-# LANGUAGE TemplateHaskell #-}

-- | Статические ресурсы Muzlovar, вшитые в бинарник.
--
-- Правки @muzlovar.css@ и @muzlovar.js@ попадают в интерфейс только
-- после пересборки: файлы не перечислены в @.cabal@, поэтому cabal
-- сам изменение не замечает — триггером перекомпиляции служит этот
-- модуль (TH-dependency от 'embedFile').
--
-- Никакого npm и сборщика фронтенда нет: CSS и JS лежат в
-- @muzlovar/static/@ и подключаются через 'Data.FileEmbed.embedFile'.
-- CSS описывает тёмную графитовую тему и трёхколоночный редактор
-- (ингредиенты · рецепт · предпросмотр) со спокойной продуктовой
-- плотностью: мягкие surface-слои вместо стопки рамок, отступы и
-- guide-линии для вложенности дерева и однострочные условия, JS строит
-- палитру и дерево (v2) по данным @/api/schema@. Drag & drop дерева —
-- собственный (pointer events, «призрак» + индикатор вставки), без
-- сторонних библиотек. Логотип и фавикон — одна и та же иконка
-- (котёл с нотой), она же branding шапки.
module Nspeller.Muzlovar.Assets
  ( muzlovarCss
  , muzlovarJs
  , logoPng
  , faviconIco
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

-- | Стили тёмной темы.
muzlovarCss :: ByteString
muzlovarCss = $(embedFile "muzlovar/static/muzlovar.css")

-- | Логика редактора (vanilla JS).
muzlovarJs :: ByteString
muzlovarJs = $(embedFile "muzlovar/static/muzlovar.js")

-- | Логотип в шапке: 128×128 PNG с прозрачным фоном (показывается
-- около 30px, запас на retina).
logoPng :: ByteString
logoPng = $(embedFile "muzlovar/static/logo.png")

-- | Фавикон: мультиразмерный .ico (16/32/48/64) из той же иконки.
faviconIco :: ByteString
faviconIco = $(embedFile "muzlovar/static/favicon.ico")
