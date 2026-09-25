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
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
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

-- | Целое число со знаком: дни и лимит.
intP :: Parser Integer
intP = lexeme intRaw <?> "целое число"
  where
    intRaw = do
      neg <- (True <$ MPC.char '-') <|> pure False
      digits <- MP.takeWhile1P (Just "цифры") isDigit
      let n = digitsToInt digits
      pure (if neg then negate n else n)

-- | Число со знаком: целая часть и необязательная дробная
-- (@1980@, @-6.5@). Значения полей и границы «между» — дробные:
-- ReplayGain в Navidrome хранится с дробной частью.
numberP :: Parser Scientific
numberP = lexeme numberRaw <?> "число"
  where
    numberRaw = do
      neg <- (True <$ MPC.char '-') <|> pure False
      whole <- MP.takeWhile1P (Just "цифры") isDigit
      frac <- MP.optional (MP.try (MPC.char '.' *> MP.takeWhile1P (Just "цифры") isDigit))
      let coeff = case frac of
            Nothing -> digitsToInt whole
            Just f -> digitsToInt whole * 10 ^ T.length f + digitsToInt f
          exponent_ = maybe 0 (negate . T.length) frac
          n = Sci.scientific coeff exponent_
      pure (if neg then negate n else n)

-- | Разряды без знака → число.
digitsToInt :: Text -> Integer
digitsToInt = T.foldl' (\acc c -> acc * 10 + toInteger (ord c - ord '0')) 0

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

-- | Отдельный символ-оператор: @=@, @>=@, @{@ и т. п. Откат
-- (@try@) нужен для операторов, являющихся началом более длинного
-- (@>=@ перед @>@) или наоборот: разбор не должен «съесть» символы
-- до момента несовпадения.
symbol :: Text -> Parser ()
symbol s = void (lexeme (MP.try (MPC.string s))) <?> T.unpack s

-- | Абсолютная дата @ГГГГ-ММ-ДД@ (ASCII-цифры и точная форма),
-- в кавычках или без.
--
-- Обе формы разбираются под 'MP.try': не-датовый операнд (@2020@ у
-- числового поля, строка не-даты после «до») не должен помешать
-- остальным альтернативам 'valueP' и сообщение ошибки остаётся
-- «ожидается дата …». Если же форма совпала, а календарной даты нет
-- (@2024-02-30@), ошибка «несуществующая дата» уже не откатывается:
-- сообщение точнее любого «ожидается …».
dateP :: Parser Day
dateP = lexeme rawDate <?> "дата в формате ГГГГ-ММ-ДД"
  where
    rawDate = do
      txt <- MP.try shape <|> MP.try (quotedShape '"')
      case parseDay txt of
        Just d -> pure d
        Nothing -> fail "несуществующая дата"

    shape = do
      y <- MP.count 4 MPC.digitChar
      void (MPC.char '-')
      m <- MP.count 2 MPC.digitChar
      void (MPC.char '-')
      d <- MP.count 2 MPC.digitChar
      pure (T.pack (y <> "-" <> m <> "-" <> d))

    -- Дата в кавычках: @"2020-01-01"@. Кавычки снимаются только
    -- когда внутри точно форма ГГГГ-ММ-ДД, иначе разбор откатывается
    -- целиком.
    quotedShape quote = do
      void (MPC.char quote)
      txt <- shape
      void (MPC.char quote)
      pure txt

-- | Значение-операнд: булево, строка, дата или число.
--
-- Дата стоит перед числом: её форма (@4 цифры @-@ @2 цифры @-@
-- @2 цифры@) не совпадает с числом, но разбор идёт слева направо
-- и незнакомый операнд должен падать на самом ожидаемом токене.
valueP :: Parser RawValue
valueP =
  MP.choice
    [ RVBool True <$ kw "да"
    , RVBool False <$ kw "нет"
    , RVText <$> stringP
    , RVDate <$> dateP
    , RVNumber <$> numberP
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
    , CICond <$> playlistP
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

-- | Начало формы @не звучало N дней@ — только проверка, без разбора.
-- Под 'MP.try': @не@ здесь съедается, и без отката следующая
-- альтернатива @<|>@ не получила бы шанса. Пробелы и переводы строк —
-- значимы лишь как разделители, поэтому после «чистого» булева
-- условия его лексема переносит позицию на следующую строку: если та
-- начинается с @не звучало@, это СЛЕДУЮЩЕЕ условие, а не оператор
-- «не содержит» у предыдущего.
notPlayedAhead :: Parser ()
notPlayedAhead = MP.try (kw "не" *> kw "звучало")

-- | Начало формы @не в подборке …@ — только проверка, без разбора.
-- Та же роль, что у 'notPlayedAhead': после булева условия без
-- значения лексема стоит на следующей строке, и @не в подборке@ там
-- является новым условием, а не оператором «не содержит» предыдущего.
playlistNotAhead :: Parser ()
playlistNotAhead = MP.try (kw "не" *> kw "в" *> kw "подборке")

-- | Членство в подборке: @в подборке id "…"@, @не в подборке файл
-- "…"@. Многословные формы фиксируются под 'MP.try': слово @в@ —
-- начало идентификатора, а @не@ может быть частью условия «поле не
-- содержит …» (оно разбирается в 'condP' после неудачи здесь).
playlistP :: Parser RawCond
playlistP =
  playlistNotAhead *> (RPlaylist NotInPlaylist <$> playlistRefP)
    <|> (kw "в" *> kw "подборке" *> (RPlaylist InPlaylist <$> playlistRefP))

-- | Вид ссылки на подборку и её значение-строка.
playlistRefP :: Parser PlaylistRef
playlistRefP =
  PlaylistRef
    <$> MP.choice [RefId <$ kw "id", RefPath <$ kw "файл"]
    <*> stringP

condP :: Parser RawCond
condP = do
  name <- ident
  condAfterFieldP name

-- | Операторы после имени поля. Многословные операторы фиксируются
-- после первого слова: это даёт точные ошибки вроде
-- «ожидается содержит» вместо догадок о намерении. Единственное
-- исключение — «не» в начале форм @не звучало@ и @не в подборке@
-- (см. 'notPlayedAhead', 'playlistNotAhead'): их нельзя принимать
-- за начало «не содержит».
condAfterFieldP :: Text -> Parser RawCond
condAfterFieldP name =
  MP.choice
    [ -- «между» двух дат — до числового «между»: @try@ откатывает
      -- разбор, если после ключевого слова стоит не дата, и числовой
      -- вариант получает шанс (@рейтинг между 1 и 2@).
      RDateBetween name <$> MP.try (kw "между" *> dateP) <*> (kw "и" *> dateP)
    , RBetween name <$> (kw "между" *> numberP) <*> (kw "и" *> numberP)
    , -- «поле не за N дней»: @не@ здесь не начало «не_contains»,
      -- поэтому форма фиксируется до разбора операторов текста.
      -- Откат до момента распознавания «не за» сохраняет разбор
      -- «поле не содержит …».
      RNotRelative name <$> MP.try (kw "не" *> kw "за" *> intP) <* kw "дней"
    , -- «до»/«после» принимают только дату: после ключевого слова
      -- разбор уже не откатывается — ошибка сразу называет формат.
      RBin name OpBefore . RVDate <$> (kw "до" *> dateP)
    , RBin name OpAfter . RVDate <$> (kw "после" *> dateP)
    , RBin name OpNotContains
        <$> ( MP.notFollowedBy (notPlayedAhead <|> playlistNotAhead)
                *> kw "не"
                *> kw "содержит"
                *> valueP
            )
    , RBin name OpStartsWith <$> (kw "начинается" *> kw "с" *> valueP)
    , RBin name OpEndsWith <$> (kw "заканчивается" *> kw "на" *> valueP)
    , RBin name OpContains <$> (kw "содержит" *> valueP)
    , RPresence name Absent <$ kw "отсутствует"
    , RPresence name Present <$ kw "присутствует"
    , RRelative name <$> (kw "за" *> intP) <* kw "дней"
    , RBin name OpNe <$> (symbol "!=" *> valueP)
    , RBin name OpEq <$> (symbol "=" *> valueP)
    , RBin name OpGe <$> (symbol ">=" *> valueP)
    , RBin name OpGt <$> (symbol ">" *> valueP)
    , RBin name OpLe <$> (symbol "<=" *> valueP)
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
