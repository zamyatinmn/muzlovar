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
  , renderParsedFileIn
  , renderStatement
  , renderCondItem
  , escapeMixString
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Nspeller.Ast
import Nspeller.Dialect

-- Nothing keeps the historical renderer byte-for-byte, including raw
-- aliases from a parsed file. Explicit dialects canonicalise field tokens.
type RenderDialect = Maybe DslDialect

word :: RenderDialect -> DslKeyword -> Text
word md k = keyword (maybe Ru id md) k

fieldToken :: RenderDialect -> Text -> Text
fieldToken Nothing n = n
fieldToken (Just d) n = case fieldByName n of
  Just (SomeField f) -> if d == Ru then fieldDslName f else fieldName f
  Nothing -> n

-- | Весь файл: секции через пустую строку не разделяются — каждая
-- секция с новой строки, группы разворачиваются с отступом.
renderParsedFile :: ParsedFile -> Text
renderParsedFile = renderParsedFileWith Nothing

renderParsedFileIn :: DslDialect -> ParsedFile -> Text
renderParsedFileIn = renderParsedFileWith . Just

renderParsedFileWith :: RenderDialect -> ParsedFile -> Text
renderParsedFileWith md (ParsedFile stmts) =
  T.intercalate "\n" (map (renderStatementWith md . locValue) stmts) <> "\n"

-- | Одна секция верхнего уровня (без позиции).
renderStatement :: Statement -> Text
renderStatement = renderStatementWith Nothing

renderStatementWith :: RenderDialect -> Statement -> Text
renderStatementWith md = \case
  SName t -> word md KPlaylist <> " " <> renderString t
  SDescription t -> word md KDescription <> " " <> renderString t
  SPublic -> word md KPublic
  SWhere g -> renderGroup md 0 (word md KWhere <> " ") g
  SSort SortRandom -> word md KSort <> " " <> word md KRandom
  SSort (SortSpec items) ->
    word md KSort <> " {\n"
      <> T.concat [indent 1 <> renderRawSortItem md (locValue i) <> "\n" | i <- items]
      <> "}"
  SLimit n -> word md KLimit <> " " <> tshow n

-- | Элемент секции сортировки: имя поля и направление.
renderRawSortItem :: RenderDialect -> RawSortItem -> Text
renderRawSortItem md (RawSortItem name dir) =
  fieldToken md name <> " " <> case dir of
    Ascending -> word md KAscending
    Descending -> word md KDescending

-- | Логическая группа с отступом @depth@ уровней (2 пробела на уровень).
renderGroup :: RenderDialect -> Int -> Text -> LogicGroup -> Text
renderGroup md depth kwText (LogicGroup kind items) =
  indent depth
    <> kwText
    <> (case kind of All -> word md KAll; Any -> word md KAny)
    <> " {\n"
    <> T.concat [renderItemAt md (depth + 1) i <> "\n" | i <- items]
    <> indent depth
    <> "}"

-- | Элемент группы: условие или вложенная группа.
renderItemAt :: RenderDialect -> Int -> Located CondItem -> Text
renderItemAt md depth (Located _ _ item) = case item of
  CICond c -> indent depth <> renderCond md c
  CIGroup g -> renderGroup md depth "" g

-- | Синоним 'renderItemAt' для экспорта (условие или группа).
renderCondItem :: Int -> CondItem -> Text
renderCondItem depth = \case
  CICond c -> indent depth <> renderCond Nothing c
  CIGroup g -> renderGroup Nothing depth "" g

-- | Условие в канонической DSL-записи.
renderCond :: RenderDialect -> RawCond -> Text
renderCond md = \case
  RBare name -> fieldToken md name
  RBin name op val -> T.unwords [fieldToken md name, renderRawOp md op, renderRawValue md val]
  RBetween name lo hi ->
    T.unwords [fieldToken md name, word md KBetween, formatNumber lo, word md KAnd, formatNumber hi]
  RPresence name op ->
    T.unwords [fieldToken md name, case op of Absent -> word md KMissing; Present -> word md KPresent]
  RRelative name days ->
    T.unwords [fieldToken md name, word md KWithin, tshow days, word md KDays]
  RNotRelative name days ->
    if md /= Nothing && isNotPlayedTarget name
      then T.unwords [word md KNotPlayed, tshow days, word md KDays]
      else T.unwords [fieldToken md name, word md KNotWithin, tshow days, word md KDays]
  RNotPlayed days ->
    T.unwords [word md KNotPlayed, tshow days, word md KDays]
  RDateBetween name lo hi ->
    T.unwords [fieldToken md name, word md KBetween, formatDay lo, word md KAnd, formatDay hi]
  RPlaylist membership ref ->
    T.unwords [case membership of InPlaylist -> word md KInPlaylist; NotInPlaylist -> word md KNotInPlaylist,
               case prKind ref of RefId -> word md KRefId; RefPath -> word md KRefFile,
               renderString (prValue ref)]

-- The historical no-field shorthand is the canonical spelling for the
-- registry's playback date in either explicitly selected dialect.
isNotPlayedTarget :: Text -> Bool
isNotPlayedTarget name = maybe False (someFieldHasCapability CapNotPlayed) (fieldByName name)

-- | Написание бинарного оператора в DSL.
renderRawOp :: RenderDialect -> RawOp -> Text
renderRawOp md = \case
  OpEq -> "="
  OpNe -> "!="
  OpGt -> ">"
  OpGe -> ">="
  OpLt -> "<"
  OpLe -> "<="
  OpContains -> word md KContains
  OpNotContains -> word md KNotContains
  OpStartsWith -> word md KStartsWith
  OpEndsWith -> word md KEndsWith
  OpBefore -> word md KBefore
  OpAfter -> word md KAfter

-- | Значение-операнд: строка всегда в кавычках, число, дата и булево
-- — как есть.
renderRawValue :: RenderDialect -> RawValue -> Text
renderRawValue md = \case
  RVText t -> renderString t
  RVNumber n -> formatNumber n
  RVBool True -> word md KTrue
  RVBool False -> word md KFalse
  RVDate d -> formatDay d

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
