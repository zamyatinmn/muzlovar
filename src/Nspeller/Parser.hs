{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Разбор DSL файлов @.mix@ в разобранный AST.
--
-- Парсер намеренно НЕ проверяет типы полей и операторов — это работа
-- 'Nspeller.Validation'. Здесь фиксируется только синтаксис и позиции
-- элементов, чтобы ошибки валидации можно было показать с точным
-- фрагментом исходника.
module Nspeller.Parser
  ( parsePlaylist
  ) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isControl, isDigit, ord)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec (Parsec, (<?>), (<|>))
import qualified Text.Megaparsec.Char as MPC
import Nspeller.Ast

-- | Тип потока DSL: символы 'Text', синтаксические ошибки без
-- компонентов пользователя.
type Parser = Parsec Void Text

------------------------------------------------------------------------------
-- Точка входа
------------------------------------------------------------------------------

-- | Разбирает содержимое файла. Синтаксические ошибки превращаются
-- в 'CompileError' с позицией и фрагментом исходника.
parsePlaylist :: FilePath -> Text -> Either CompileError ParsedFile
parsePlaylist fp src =
  case MP.runParser fileP fp src of
    Left bundle -> Left (bundleToError fp src bundle)
    Right stmts -> Right (ParsedFile stmts)

-- | Секции верхнего уровня; пустой файл здесь не ошибка — об отсутствии
-- обязательных секций сообщит валидация.
fileP :: Parser [Located Statement]
fileP = spaceConsumer *> MP.many (located statement) <* MP.eof

-- | Оборачивает парсер, запоминая смещения начала и конца элемента.
located :: Parser a -> Parser (Located a)
located p = do
  start <- MP.getOffset
  value <- p
  end <- MP.getOffset
  pure (Located start end value)

------------------------------------------------------------------------------
-- Лексика
------------------------------------------------------------------------------

-- | Пробелы, переводы строк (включая CRLF) и однострочные комментарии
-- вида @\#@ …@ до конца строки.
spaceConsumer :: Parser ()
spaceConsumer = go
  where
    go = do
      _ <- MP.takeWhileP (Just "пробел") isSpaceChar
      mc <- MP.optional (MPC.char '#')
      case mc of
        Just _ -> void (MP.takeWhileP Nothing (/= '\n')) >> go
        Nothing -> pure ()

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

lexeme :: Parser a -> Parser a
lexeme p = p <* spaceConsumer

-- | Ключевое слово: целое слово, не являющееся началом более длинного
-- идентификатора (@и@ не съедает @итд@).
kw :: Text -> Parser ()
kw w = lexeme (void (MP.try (MPC.string w <* notIdentChar)))
  where
    notIdentChar = MP.notFollowedBy (MP.satisfy isIdentChar <?> "идентификатор")

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

-- | Идентификатор: имя поля или слово DSL.
ident :: Parser Text
ident = lexeme (MP.takeWhile1P (Just "идентификатор") isIdentChar) <?> "идентификатор"

-- | Целое число со знаком.
intP :: Parser Integer
intP = lexeme intRaw <?> "целое число"
  where
    intRaw = do
      neg <- (True <$ MPC.char '-') <|> pure False
      digits <- MP.takeWhile1P (Just "цифры") isDigit
      let n = T.foldl' (\acc c -> acc * 10 + toInteger (ord c - ord '0')) 0 digits
      pure (if neg then negate n else n)

-- | Символ внутри строкового литерала: обычный символ (кроме кавычки
-- и управляющих) либо экранированный: \", \\, \n, \t, \r.
charInString :: Parser Char
charInString =
  MP.choice
    [ MPC.char '\\' *> MP.choice escape
    , MP.satisfy (\c -> c /= '"' && not (isControl c))
    ]
    <?> "символ строки"
  where
    escape =
      [ '"' <$ MPC.char '"'
      , '\\' <$ MPC.char '\\'
      , '\n' <$ MPC.char 'n'
      , '\t' <$ MPC.char 't'
      , '\r' <$ MPC.char 'r'
      ]

-- | Строковый литерал с экранированиями.
stringP :: Parser Text
stringP =
  lexeme (T.pack <$> (MPC.char '"' *> MP.manyTill charInString (MPC.char '"')))
    <?> "строка"

-- | Отдельный символ-оператор: @=@, @>@, @{@ и т. п.
symbol :: Text -> Parser ()
symbol s = void (lexeme (MPC.string s)) <?> T.unpack s

-- | Значение-операнд: булево, строка или число.
valueP :: Parser RawValue
valueP =
  MP.choice
    [ RVBool True <$ kw "да"
    , RVBool False <$ kw "нет"
    , RVText <$> stringP
    , RVNumber <$> intP
    ]

------------------------------------------------------------------------------
-- Секции верхнего уровня
------------------------------------------------------------------------------

statement :: Parser Statement
statement =
  MP.choice
    [ SName <$> (kw "подборка" *> stringP)
    , SDescription <$> (kw "описание" *> stringP)
    , SPublic <$ kw "публичная"
    , SWhere <$> (kw "где" *> logicGroupP)
    , SSort <$> sortSectionP
    , SLimit <$> (kw "лимит" *> intP)
    ]

-- | Группа условий: обязательные фигурные скобки и хотя бы одно условие.
logicGroupP :: Parser LogicGroup
logicGroupP = do
  kind <- MP.choice [All <$ kw "все", Any <$ kw "любое"] <?> "«все» или «любое»"
  items <- symbol "{" *> MP.some (located condItemP) <* symbol "}"
  pure (LogicGroup kind items)

sortSectionP :: Parser RawSort
sortSectionP = do
  kw "порядок"
  MP.choice
    [ SortRandom <$ kw "случайный"
    , SortSpec <$> (symbol "{" *> MP.some (located sortItemP) <* symbol "}")
    ]
    <?> "«случайный» или блок с полями"

sortItemP :: Parser RawSortItem
sortItemP = do
  name <- ident
  dir <- MP.choice [Descending <$ kw "убыв", Ascending <$ kw "возр"] <?> "«возр» или «убыв»"
  pure (RawSortItem name dir)

------------------------------------------------------------------------------
-- Условия
------------------------------------------------------------------------------

condItemP :: Parser CondItem
condItemP =
  MP.choice
    [ CIGroup <$> logicGroupP
    , CICond <$> notPlayedP
    , CICond <$> condP
    ]
    <?> "условие"

-- | @не звучало 90 дней@. Откат (@try@) нужен только до момента
-- распознавания сочетания «не звучало»: слово @не@ может быть началом
-- идентификатора или частью условия вида «поле не содержит …».
notPlayedP :: Parser RawCond
notPlayedP =
  MP.try (kw "не" *> kw "звучало")
    *> (RNotPlayed <$> intP)
    <* kw "дней"

condP :: Parser RawCond
condP = do
  name <- ident
  condAfterFieldP name

-- | Операторы после имени поля. Многословные операторы фиксируются
-- после первого слова: это даёт точные ошибки вроде
-- «ожидается содержит» вместо догадок о намерении.
condAfterFieldP :: Text -> Parser RawCond
condAfterFieldP name =
  MP.choice
    [ RBetween name <$> (kw "между" *> intP) <*> (kw "и" *> intP)
    , RBin name OpNotContains <$> (kw "не" *> kw "содержит" *> valueP)
    , RBin name OpStartsWith <$> (kw "начинается" *> kw "с" *> valueP)
    , RBin name OpEndsWith <$> (kw "заканчивается" *> kw "на" *> valueP)
    , RBin name OpContains <$> (kw "содержит" *> valueP)
    , RPresence name Absent <$ kw "отсутствует"
    , RPresence name Present <$ kw "присутствует"
    , RRelative name <$> (kw "за" *> intP) <* kw "дней"
    , RBin name OpNe <$> (symbol "!=" *> valueP)
    , RBin name OpEq <$> (symbol "=" *> valueP)
    , RBin name OpGt <$> (symbol ">" *> valueP)
    , RBin name OpLt <$> (symbol "<" *> valueP)
    , pure (RBare name)
    ]

------------------------------------------------------------------------------
-- Ошибки разбора
------------------------------------------------------------------------------

-- | Превращает позиционную ошибку megaparsec в 'CompileError' с
-- форматированием, одинаковым с ошибками валидации.
bundleToError :: FilePath -> Text -> MP.ParseErrorBundle Text Void -> CompileError
bundleToError fp src bundle =
  let (decorated, _) =
        MP.attachSourcePos
          MP.errorOffset
          (MP.bundleErrors bundle)
          (MP.bundlePosState bundle)
      (err, sourcePos) = NE.head decorated
      ln = MP.unPos (MP.sourceLine sourcePos)
      col = MP.unPos (MP.sourceColumn sourcePos)
      lineText = lineTextAt src ln
      restOfLine = T.stripEnd (T.drop (col - 1) lineText)
      spanW = max 1 (T.length restOfLine)
   in CompileError fp (Just (ln, col)) lineText spanW (errorTextMessages err)

-- | Сообщение синтаксической ошибки построчно.
errorTextMessages :: MP.ParseError Text Void -> NonEmpty Text
errorTextMessages err =
  case filter (not . T.null) (map T.strip (T.splitOn "\n" (T.pack pretty))) of
    [] -> "Синтаксическая ошибка." :| []
    (x : xs) -> x :| xs
  where
    pretty = MP.parseErrorTextPretty err
