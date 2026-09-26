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
  , parsePlaylistIn
  , detectDslDialect
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
import Nspeller.Dialect

-- | Тип потока DSL: символы 'Text', синтаксические ошибки без
-- компонентов пользователя.
type Parser = Parsec Void Text

------------------------------------------------------------------------------
-- Точка входа
------------------------------------------------------------------------------

-- | Разбирает содержимое файла. Синтаксические ошибки превращаются
-- в 'CompileError' с позицией и фрагментом исходника.
parsePlaylist :: FilePath -> Text -> Either CompileError ParsedFile
parsePlaylist = parsePlaylistWith [Ru, En]

-- | Restricted mode is used only to identify a uniformly written source.
-- The public parser accepts both dialects, including mixed documents.
parsePlaylistIn :: DslDialect -> FilePath -> Text -> Either CompileError ParsedFile
parsePlaylistIn d = parsePlaylistWith [d]

-- | A mixed or unrecognised source has no single dialect. This is used
-- only for display preferences; parsing itself accepts mixed syntax.
detectDslDialect :: FilePath -> Text -> Maybe DslDialect
detectDslDialect fp src = case [d | d <- [Ru, En], matches d] of
  [d] -> Just d
  _ -> Nothing
  where
    matches d = case parsePlaylistIn d fp src of
      Right (ParsedFile stmts) -> all (statementFields d . locValue) stmts
      Left _ -> False
    statementFields d = \case
      SWhere g -> groupFields d g
      SSort (SortSpec xs) -> all (sortField d . locValue) xs
      _ -> True
    groupFields d (LogicGroup _ xs) = all (itemFields d . locValue) xs
    itemFields d = \case
      CIGroup g -> groupFields d g
      CICond c -> maybe True (fieldToken d) (condName c)
    sortField d (RawSortItem name _) = fieldToken d name
    condName = \case
      RBare n -> Just n
      RBin n _ _ -> Just n
      RBetween n _ _ -> Just n
      RPresence n _ -> Just n
      RRelative n _ -> Just n
      RNotRelative n _ -> Just n
      RDateBetween n _ _ -> Just n
      _ -> Nothing
    fieldToken d n = case fieldByName n of
      Just (SomeField f)
        | fieldDslName f == fieldName f -> True
        | d == Ru -> n /= fieldName f
        | otherwise -> n /= fieldDslName f
      Nothing -> False

parsePlaylistWith :: [DslDialect] -> FilePath -> Text -> Either CompileError ParsedFile
parsePlaylistWith dialects fp src =
  case MP.runParser (fileP dialects) fp src of
    Left bundle -> Left (bundleToError fp src bundle)
    Right stmts -> Right (ParsedFile stmts)

-- | Секции верхнего уровня; пустой файл здесь не ошибка — об отсутствии
-- обязательных секций сообщит валидация.
fileP :: [DslDialect] -> Parser [Located Statement]
fileP ds = spaceConsumer *> MP.many (located (statement ds)) <* MP.eof

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

-- | Parse an entire phrase, with normal DSL whitespace between words.
-- Trying the complete phrase prevents a short prefix from committing a
-- longer operator (for example, Russian «не» or English «not»).
kwDsl :: [DslDialect] -> DslKeyword -> Parser ()
kwDsl ds key = MP.choice [MP.try (mapM_ kw (T.words (keyword d key))) | d <- ds]

daysDsl :: [DslDialect] -> Parser ()
daysDsl ds = kwDsl ds KDays <|> (if En `elem` ds then kw "day" else MP.empty)

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
valueP :: [DslDialect] -> Parser RawValue
valueP ds =
  MP.choice
    [ RVBool True <$ kwDsl ds KTrue
    , RVBool False <$ kwDsl ds KFalse
    , RVText <$> stringP
    , RVDate <$> dateP
    , RVNumber <$> numberP
    ]

------------------------------------------------------------------------------
-- Секции верхнего уровня
------------------------------------------------------------------------------

statement :: [DslDialect] -> Parser Statement
statement ds =
  MP.choice
    [ SName <$> (kwDsl ds KPlaylist *> stringP)
    , SDescription <$> (kwDsl ds KDescription *> stringP)
    , SPublic <$ kwDsl ds KPublic
    , SWhere <$> (kwDsl ds KWhere *> logicGroupP ds)
    , SSort <$> sortSectionP ds
    , SLimit <$> (kwDsl ds KLimit *> intP)
    ]

-- | Группа условий: обязательные фигурные скобки и хотя бы одно условие.
logicGroupP :: [DslDialect] -> Parser LogicGroup
logicGroupP ds = do
  kind <- MP.choice [All <$ kwDsl ds KAll, Any <$ kwDsl ds KAny] <?> "«все»/«all» или «любое»/«any»"
  items <- symbol "{" *> MP.some (located (condItemP ds)) <* symbol "}"
  pure (LogicGroup kind items)

sortSectionP :: [DslDialect] -> Parser RawSort
sortSectionP ds = do
  kwDsl ds KSort
  MP.choice
    [ SortRandom <$ kwDsl ds KRandom
    , SortSpec <$> (symbol "{" *> MP.some (located (sortItemP ds)) <* symbol "}")
    ]
    <?> "«случайный» или блок с полями"

sortItemP :: [DslDialect] -> Parser RawSortItem
sortItemP ds = do
  name <- ident
  dir <- MP.choice [Descending <$ kwDsl ds KDescending, Ascending <$ kwDsl ds KAscending] <?> "направление сортировки"
  pure (RawSortItem name dir)

------------------------------------------------------------------------------
-- Условия
------------------------------------------------------------------------------

condItemP :: [DslDialect] -> Parser CondItem
condItemP ds =
  MP.choice
    [ CIGroup <$> logicGroupP ds
    , CICond <$> notPlayedP ds
    , CICond <$> playlistP ds
    , CICond <$> condP ds
    ]
    <?> "условие"

-- | @не звучало 90 дней@. Откат (@try@) нужен только до момента
-- распознавания сочетания «не звучало»: слово @не@ может быть началом
-- идентификатора или частью условия вида «поле не содержит …».
notPlayedP :: [DslDialect] -> Parser RawCond
notPlayedP ds =
  kwDsl ds KNotPlayed
    *> (RNotPlayed <$> intP)
    <* daysDsl ds

-- | Начало формы @не звучало N дней@ — только проверка, без разбора.
-- Под 'MP.try': @не@ здесь съедается, и без отката следующая
-- альтернатива @<|>@ не получила бы шанса. Пробелы и переводы строк —
-- значимы лишь как разделители, поэтому после «чистого» булева
-- условия его лексема переносит позицию на следующую строку: если та
-- начинается с @не звучало@, это СЛЕДУЮЩЕЕ условие, а не оператор
-- «не содержит» у предыдущего.
notPlayedAhead :: [DslDialect] -> Parser ()
notPlayedAhead ds = MP.try (kwDsl ds KNotPlayed)

-- | Начало формы @не в подборке …@ — только проверка, без разбора.
-- Та же роль, что у 'notPlayedAhead': после булева условия без
-- значения лексема стоит на следующей строке, и @не в подборке@ там
-- является новым условием, а не оператором «не содержит» предыдущего.
playlistNotAhead :: [DslDialect] -> Parser ()
playlistNotAhead ds = MP.try (kwDsl ds KNotInPlaylist)

-- | Членство в подборке: @в подборке id "…"@, @не в подборке файл
-- "…"@. Многословные формы фиксируются под 'MP.try': слово @в@ —
-- начало идентификатора, а @не@ может быть частью условия «поле не
-- содержит …» (оно разбирается в 'condP' после неудачи здесь).
playlistP :: [DslDialect] -> Parser RawCond
playlistP ds =
  playlistNotAhead ds *> (RPlaylist NotInPlaylist <$> playlistRefP ds)
    <|> (kwDsl ds KInPlaylist *> (RPlaylist InPlaylist <$> playlistRefP ds))

-- | Вид ссылки на подборку и её значение-строка.
playlistRefP :: [DslDialect] -> Parser PlaylistRef
playlistRefP ds =
  PlaylistRef
    <$> MP.choice [RefId <$ kwDsl ds KRefId, RefPath <$ kwDsl ds KRefFile]
    <*> stringP

condP :: [DslDialect] -> Parser RawCond
condP ds = do
  name <- ident
  condAfterFieldP ds name

-- | Операторы после имени поля. Многословные операторы фиксируются
-- после первого слова: это даёт точные ошибки вроде
-- «ожидается содержит» вместо догадок о намерении. Единственное
-- исключение — «не» в начале форм @не звучало@ и @не в подборке@
-- (см. 'notPlayedAhead', 'playlistNotAhead'): их нельзя принимать
-- за начало «не содержит».
condAfterFieldP :: [DslDialect] -> Text -> Parser RawCond
condAfterFieldP ds name =
  MP.choice
    [ -- «между» двух дат — до числового «между»: @try@ откатывает
      -- разбор, если после ключевого слова стоит не дата, и числовой
      -- вариант получает шанс (@рейтинг между 1 и 2@).
      RDateBetween name <$> MP.try (kwDsl ds KBetween *> dateP) <*> (kwDsl ds KAnd *> dateP)
    , RBetween name <$> (kwDsl ds KBetween *> numberP) <*> (kwDsl ds KAnd *> numberP)
    , -- «поле не за N дней»: @не@ здесь не начало «не_contains»,
      -- поэтому форма фиксируется до разбора операторов текста.
      -- Откат до момента распознавания «не за» сохраняет разбор
      -- «поле не содержит …».
      RNotRelative name <$> (kwDsl ds KNotWithin *> intP) <* daysDsl ds
    , -- «до»/«после» принимают только дату: после ключевого слова
      -- разбор уже не откатывается — ошибка сразу называет формат.
      RBin name OpBefore . RVDate <$> (kwDsl ds KBefore *> dateP)
    , RBin name OpAfter . RVDate <$> (kwDsl ds KAfter *> dateP)
    , RBin name OpNotContains
        <$> ( MP.notFollowedBy (notPlayedAhead ds <|> playlistNotAhead ds)
                *> kwDsl ds KNotContains
                *> valueP ds
            )
    , RBin name OpStartsWith <$> (kwDsl ds KStartsWith *> valueP ds)
    , RBin name OpEndsWith <$> (kwDsl ds KEndsWith *> valueP ds)
    , RBin name OpContains <$> (kwDsl ds KContains *> valueP ds)
    , RPresence name Absent <$ kwDsl ds KMissing
    , RPresence name Present <$ kwDsl ds KPresent
    , RRelative name <$> (kwDsl ds KWithin *> intP) <* daysDsl ds
    , RBin name OpNe <$> (symbol "!=" *> valueP ds)
    , RBin name OpEq <$> (symbol "=" *> valueP ds)
    , RBin name OpGe <$> (symbol ">=" *> valueP ds)
    , RBin name OpGt <$> (symbol ">" *> valueP ds)
    , RBin name OpLe <$> (symbol "<=" *> valueP ds)
    , RBin name OpLt <$> (symbol "<" *> valueP ds)
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
