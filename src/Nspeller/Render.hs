{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Канонический рендер разобранного AST обратно в текст @.mix@.
--
-- Обратная операция к 'Nspeller.Parser.parsePlaylist': для любого
-- разобранного файла выполняется свойство
--
-- @
-- parsePlaylist fp (renderParsedFile ast) == Right ast'  ==  ast' ≡ ast
-- @
--
-- с точностью до позиций ('Nspeller.Ast.Located'). Рендер не меняет
-- порядок секций и элементов и не добавляет семантику: форматирование
-- (отступы, переводы строк) канонично, но не значимо.
module Nspeller.Render
  ( renderParsedFile
  , renderStatement
  , renderCondItem
  , escapeMixString
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Nspeller.Ast

-- | Весь файл: секции через пустую строку не разделяются — каждая
-- секция с новой строки, группы разворачиваются с отступом.
renderParsedFile :: ParsedFile -> Text
renderParsedFile (ParsedFile stmts) =
  T.intercalate "\n" (map (renderStatement . locValue) stmts) <> "\n"

-- | Одна секция верхнего уровня (без позиции).
renderStatement :: Statement -> Text
renderStatement = \case
  SName t -> "подборка " <> renderString t
  SDescription t -> "описание " <> renderString t
  SPublic -> "публичная"
  SWhere g -> renderGroup 0 "где " g
  SSort SortRandom -> "порядок случайный"
  SSort (SortSpec items) ->
    "порядок {\n"
      <> T.concat [indent 1 <> renderRawSortItem (locValue i) <> "\n" | i <- items]
      <> "}"
  SLimit n -> "лимит " <> tshow n

-- | Элемент секции сортировки: имя поля и направление.
renderRawSortItem :: RawSortItem -> Text
renderRawSortItem (RawSortItem name dir) =
  name <> " " <> case dir of
    Ascending -> "возр"
    Descending -> "убыв"

-- | Логическая группа с отступом @depth@ уровней (2 пробела на уровень).
renderGroup :: Int -> Text -> LogicGroup -> Text
renderGroup depth kwText (LogicGroup kind items) =
  indent depth
    <> kwText
    <> (case kind of All -> "все"; Any -> "любое")
    <> " {\n"
    <> T.concat [renderItemAt (depth + 1) i <> "\n" | i <- items]
    <> indent depth
    <> "}"

-- | Элемент группы: условие или вложенная группа.
renderItemAt :: Int -> Located CondItem -> Text
renderItemAt depth (Located _ _ item) = case item of
  CICond c -> indent depth <> renderCond c
  CIGroup g -> renderGroup depth "" g

-- | Синоним 'renderItemAt' для экспорта (условие или группа).
renderCondItem :: Int -> CondItem -> Text
renderCondItem depth = \case
  CICond c -> indent depth <> renderCond c
  CIGroup g -> renderGroup depth "" g

-- | Условие в канонической DSL-записи.
renderCond :: RawCond -> Text
renderCond = \case
  RBare name -> name
  RBin name op val -> T.unwords [name, renderRawOp op, renderRawValue val]
  RBetween name lo hi ->
    T.unwords [name, "между", tshow lo, "и", tshow hi]
  RPresence name op ->
    T.unwords [name, presenceOpDesc op]
  RRelative name days ->
    T.unwords [name, "за", tshow days, "дней"]
  RNotPlayed days ->
    T.unwords ["не", "звучало", tshow days, "дней"]

-- | Написание бинарного оператора в DSL.
renderRawOp :: RawOp -> Text
renderRawOp = \case
  OpEq -> "="
  OpNe -> "!="
  OpGt -> ">"
  OpLt -> "<"
  OpContains -> "содержит"
  OpNotContains -> "не содержит"
  OpStartsWith -> "начинается с"
  OpEndsWith -> "заканчивается на"

-- | Значение-операнд: строка всегда в кавычках, число и булево — как есть.
renderRawValue :: RawValue -> Text
renderRawValue = \case
  RVText t -> renderString t
  RVNumber n -> tshow n
  RVBool True -> "да"
  RVBool False -> "нет"

-- | Строковый литерал с экранированием символов, которые парсер
-- принимает только в экранированной форме.
renderString :: Text -> Text
renderString t = "\"" <> escapeMixString t <> "\""

-- | Экранирование содержимого строкового литерала DSL.
escapeMixString :: Text -> Text
escapeMixString = T.concatMap escape
  where
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\r' -> "\\r"
      c -> T.singleton c

indent :: Int -> Text
indent n = T.replicate (2 * n) " "

tshow :: Show a => a -> Text
tshow = T.pack . show
