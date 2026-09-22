{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Семантическая проверка разобранного AST.
--
-- Валидация отвечает за все категории ошибок, кроме синтаксических:
--
-- * неизвестные поля;
-- * операторы, несовместимые с типом поля;
-- * некорректные значения;
-- * дублирующиеся и отсутствующие секции;
-- * неположительный лимит;
-- * недопустимые поля сортировки.
--
-- Все ошибки файла собираются целиком (не только первая), после чего
-- строится типобезопасный 'ValidPlaylist'.
module Nspeller.Validation
  ( validatePlaylist
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Nspeller.Ast

------------------------------------------------------------------------------
-- Точка входа
------------------------------------------------------------------------------

-- | Проверяет разобранный файл и строит валидированный AST.
--
-- Инвариант: 'Left' всегда содержит хотя бы одну ошибку.
validatePlaylist :: FilePath -> Text -> ParsedFile -> Either [CompileError] ValidPlaylist
validatePlaylist fp src (ParsedFile stmts) =
  let nameR = reqSection "подборка" projName
      descR = optSection "описание" projDesc
      publicR = optSection "публичная" projPublic
      whereR = reqSection "где" projWhere
      sortR = optSection "порядок" projSort
      limitR = optSection "лимит" projLimit

      groupR = case whereR of
        Left errs -> Left errs
        Right lg -> validateGroup fp src (locValue lg)

      sortChecked = case sortR of
        Left errs -> Left errs
        Right Nothing -> Right Nothing
        Right (Just l) -> case validateSort fp src (locValue l) of
          Left errs -> Left errs
          Right mode -> Right (Just mode)

      limitChecked = case limitR of
        Left errs -> Left errs
        Right Nothing -> Right Nothing
        Right (Just l)
          | locValue l > 0 -> Right (Just (locValue l))
          | otherwise ->
              Left
                [ errorAt fp src l $
                    "Значение лимита должно быть положительным целым числом, получено "
                      <> tshow (locValue l)
                      <> "."
                      :| []
                ]

      allErrs =
        errsOf nameR
          ++ errsOf descR
          ++ errsOf publicR
          ++ errsOf groupR
          ++ errsOf sortChecked
          ++ errsOf limitChecked
   in case (nameR, descR, publicR, groupR, sortChecked, limitChecked) of
        (Right name, Right desc, Right pub, Right grp, Right srt, Right lim)
          | null allErrs ->
              Right
                ValidPlaylist
                  { vpName = locValue name
                  , vpDescription = locValue <$> desc
                  , vpPublic = isJust pub
                  , vpRoot = grp
                  , vpSort = srt
                  , vpLimit = lim
                  }
        -- Ветка недостижима: значение отсутствует тогда и только тогда,
        -- когда есть ошибки. Защита нужна лишь для полноты (без
        -- частичных функций вроде fromJust).
        _ -> Left (if null allErrs then [fileError fp "внутренняя ошибка компиляции"] else allErrs)
  where
    errsOf :: Either [CompileError] a -> [CompileError]
    errsOf = either id (const [])

    tshow :: Show b => b -> Text
    tshow = T.pack . show

    -- * Разбор секций ------------------------------------------------------

    collect :: Text -> (Statement -> Maybe a) -> ([Located a], [CompileError])
    collect label proj =
      let hits =
            [ Located (locStart s) (locEnd s) a
            | s <- stmts
            , Just a <- [proj (locValue s)]
            ]
          dups =
            [ errorAt fp src h ("Секция «" <> label <> "» указана повторно." :| [])
            | h <- drop 1 hits
            ]
       in (hits, dups)

    reqSection :: Text -> (Statement -> Maybe a) -> Either [CompileError] (Located a)
    reqSection label proj = case collect label proj of
      ([], []) ->
        Left [errorAtFileStart fp src ("Отсутствует обязательная секция «" <> label <> "»." :| [])]
      ([], errs) -> Left errs
      (hit : _, []) -> Right hit
      (_ : _, errs) -> Left errs

    optSection :: Text -> (Statement -> Maybe a) -> Either [CompileError] (Maybe (Located a))
    optSection label proj = case collect label proj of
      ([], []) -> Right Nothing
      ([], errs) -> Left errs
      (hit : _, []) -> Right (Just hit)
      (_ : _, errs) -> Left errs

    projName :: Statement -> Maybe Text
    projName (SName t) = Just t
    projName _ = Nothing

    projDesc :: Statement -> Maybe Text
    projDesc (SDescription t) = Just t
    projDesc _ = Nothing

    projPublic :: Statement -> Maybe ()
    projPublic SPublic = Just ()
    projPublic _ = Nothing

    projWhere :: Statement -> Maybe LogicGroup
    projWhere (SWhere g) = Just g
    projWhere _ = Nothing

    projSort :: Statement -> Maybe RawSort
    projSort (SSort s) = Just s
    projSort _ = Nothing

    projLimit :: Statement -> Maybe Integer
    projLimit (SLimit n) = Just n
    projLimit _ = Nothing

------------------------------------------------------------------------------
-- Группы условий
------------------------------------------------------------------------------

-- | Накопление ошибок: в отличие от @Either@'а, здесь сохраняются
-- ошибки всех дочерних элементов, а не только первой.
step :: [Either [CompileError] a] -> Either [CompileError] [a]
step xs = case [errs | Left errs <- xs] of
  [] -> Right [a | Right a <- xs]
  errss -> Left (concat errss)

validateGroup :: FilePath -> Text -> LogicGroup -> Either [CompileError] ValidGroup
validateGroup fp src (LogicGroup kind items) =
  case step (map (validateItem fp src) items) of
    Left errs -> Left errs
    Right vals -> Right (ValidGroup kind vals)

validateItem :: FilePath -> Text -> Located CondItem -> Either [CompileError] ValidItem
validateItem fp src li = case locValue li of
  CICond raw -> case validateCond fp src (Located (locStart li) (locEnd li) raw) of
    Left errs -> Left errs
    Right c -> Right (VIC c)
  CIGroup grp -> case validateGroup fp src grp of
    Left errs -> Left errs
    Right g -> Right (VIG g)

------------------------------------------------------------------------------
-- Условия
------------------------------------------------------------------------------

validateCond :: FilePath -> Text -> Located RawCond -> Either [CompileError] ValidCond
validateCond fp src lc = case validateCond1 (locValue lc) of
  Left msgs -> Left [errorAt fp src lc msgs]
  Right valid -> Right valid

-- | Проверка одного условия без позиции: позицию добавит 'validateCond'.
validateCond1 :: RawCond -> Either (NonEmpty Text) ValidCond
validateCond1 = \case
  RBare name -> withField name $ \fld -> case fld of
    Loved -> Right (VBool fld True)
    HasCoverArt -> Right (VBool fld True)
    _ -> Left (bareErr name (fieldValueType fld))
  RBin name op val -> withField name $ \fld -> validateBin name fld op val
  RBetween name lo hi -> withField name $ \fld -> case fld of
    Year -> betweenNum lo hi fld
    Rating -> betweenNum lo hi fld
    PlayCount -> betweenNum lo hi fld
    RGTrackGain -> betweenNum lo hi fld
    _ -> Left (numOnlyErr "между" name (fieldValueType fld))
  RPresence name pop -> withField name $ \fld ->
    case fieldPresence fld of
      Just p -> Right (VPresence p pop)
      Nothing -> Left (presenceErr (presenceOpDesc pop) name)
  RRelative name days -> withField name $ \fld -> case fld of
    LastPlayed
      | days > 0 -> Right (VRelative fld InTheLast days)
      | otherwise -> Left (daysErr days)
    DateAdded
      | days > 0 -> Right (VRelative fld InTheLast days)
      | otherwise -> Left (daysErr days)
    _ -> Left (dateOnlyErr name (fieldValueType fld))
  RNotPlayed days
    | days > 0 -> Right (VRelative LastPlayed NotInTheLast days)
    | otherwise -> Left (daysErr days)
  where
    withField ::
      Text ->
      (forall a. Field a -> Either (NonEmpty Text) ValidCond) ->
      Either (NonEmpty Text) ValidCond
    withField name k = case fieldByName name of
      Nothing -> Left (unknownFieldErr name)
      Just (SomeField fld) -> k fld

    betweenNum :: Integer -> Integer -> Field Integer -> Either (NonEmpty Text) ValidCond
    betweenNum lo hi f
      | lo <= hi = Right (VBetween f lo hi)
      | otherwise = Left (rangeErr lo hi)

-- | Проверка бинарного условия: сначала тип поля, затем тип значения.
validateBin ::
  Text ->
  Field a ->
  RawOp ->
  RawValue ->
  Either (NonEmpty Text) ValidCond
validateBin name fld op val = case fld of
  Title -> binT fld
  Album -> binT fld
  Genre -> binT fld
  ExplicitStatus -> binT fld
  Year -> binN fld
  Rating -> binN fld
  PlayCount -> binN fld
  RGTrackGain -> binN fld
  Loved -> binB fld
  HasCoverArt -> binB fld
  LastPlayed -> binD
  DateAdded -> binD
  where
    -- Сопоставление по конструктору GADT выше уточнило индекс поля,
    -- поэтому сборка 'ValidCond' в ветках типобезопасна.

    binT :: Field Text -> Either (NonEmpty Text) ValidCond
    binT f = case op of
      OpEq -> tEq False
      OpNe -> tEq True
      OpGt -> Left (numOnlyErr (rawOpDesc op) name TextType)
      OpLt -> Left (numOnlyErr (rawOpDesc op) name TextType)
      OpContains -> tLike TContains
      OpNotContains -> tLike TNotContains
      OpStartsWith -> tLike TStartsWith
      OpEndsWith -> tLike TEndsWith
      where
        tEq negated = case val of
          RVText t -> Right (VText f (if negated then TNe else TEq) t)
          _ -> Left (valueMismatchErr (rawValueText val) name TextType)

        tLike top = case val of
          RVText t -> Right (VText f top t)
          _ -> Left (operandErr (rawOpDesc op) "текстовое" (rawValueText val))

    binN :: Field Integer -> Either (NonEmpty Text) ValidCond
    binN f = case op of
      OpEq -> nEq False
      OpNe -> nEq True
      OpGt -> nCmp NGt
      OpLt -> nCmp NLt
      OpContains -> Left (textOnlyErr (rawOpDesc op) name NumberType)
      OpNotContains -> Left (textOnlyErr (rawOpDesc op) name NumberType)
      OpStartsWith -> Left (textOnlyErr (rawOpDesc op) name NumberType)
      OpEndsWith -> Left (textOnlyErr (rawOpDesc op) name NumberType)
      where
        nEq negated = case val of
          RVNumber n -> Right (VNumber f (if negated then NNe else NEq) n)
          _ -> Left (valueMismatchErr (rawValueText val) name NumberType)

        nCmp ctor = case val of
          RVNumber n -> Right (VNumber f ctor n)
          _ -> Left (operandErr (rawOpDesc op) "числовое" (rawValueText val))

    binB :: Field Bool -> Either (NonEmpty Text) ValidCond
    binB f = case op of
      OpEq -> bEq False
      OpNe -> bEq True
      OpGt -> Left (numOnlyErr (rawOpDesc op) name BoolType)
      OpLt -> Left (numOnlyErr (rawOpDesc op) name BoolType)
      OpContains -> Left (textOnlyErr (rawOpDesc op) name BoolType)
      OpNotContains -> Left (textOnlyErr (rawOpDesc op) name BoolType)
      OpStartsWith -> Left (textOnlyErr (rawOpDesc op) name BoolType)
      OpEndsWith -> Left (textOnlyErr (rawOpDesc op) name BoolType)
      where
        bEq negated = case val of
          RVBool b -> Right (VBool f (if negated then not b else b))
          _ -> Left (valueMismatchErr (rawValueText val) name BoolType)

    binD :: Either (NonEmpty Text) ValidCond
    binD = case op of
      OpEq -> Left (dateEqErr name)
      OpNe -> Left (dateEqErr name)
      OpGt -> Left (numOnlyErr (rawOpDesc op) name DateType)
      OpLt -> Left (numOnlyErr (rawOpDesc op) name DateType)
      OpContains -> Left (textOnlyErr (rawOpDesc op) name DateType)
      OpNotContains -> Left (textOnlyErr (rawOpDesc op) name DateType)
      OpStartsWith -> Left (textOnlyErr (rawOpDesc op) name DateType)
      OpEndsWith -> Left (textOnlyErr (rawOpDesc op) name DateType)

------------------------------------------------------------------------------
-- Сортировка
------------------------------------------------------------------------------

validateSort :: FilePath -> Text -> RawSort -> Either [CompileError] SortMode
validateSort fp src = \case
  SortRandom -> Right SortRandomMode
  SortSpec items -> SortBy <$> step (map (validateSortItem fp src) items)

validateSortItem :: FilePath -> Text -> Located RawSortItem -> Either [CompileError] SortItem
validateSortItem fp src li = case locValue li of
  RawSortItem name dir ->
    case sortFieldByName name of
      Just sf -> Right (SortItem sf (toSortDir dir))
      Nothing ->
        let boolField = case fieldByName name of
              Just (SomeField fld) | fieldValueType fld == BoolType -> True
              _ -> False
            msg
              | boolField =
                  "Булево поле «" <> name <> "» нельзя использовать для сортировки." :| []
              | otherwise =
                  "Неизвестное поле сортировки «" <> name <> "»." :| []
         in Left [errorAt fp src li msg]
  where
    toSortDir Ascending = SortAsc
    toSortDir Descending = SortDesc

------------------------------------------------------------------------------
-- Сообщения об ошибках
------------------------------------------------------------------------------

unknownFieldErr :: Text -> NonEmpty Text
unknownFieldErr name = "Неизвестное поле «" <> name <> "»." :| []

bareErr :: Text -> ValueType -> NonEmpty Text
bareErr name vt =
  "Сокращённая запись допустима только для логических полей."
    :| ["Поле «" <> name <> "» имеет " <> valueTypeDesc vt <> "."]

textOnlyErr :: Text -> Text -> ValueType -> NonEmpty Text
textOnlyErr op name vt =
  "Оператор «" <> op <> "» применим только к текстовым полям."
    :| ["Поле «" <> name <> "» имеет " <> valueTypeDesc vt <> "."]

numOnlyErr :: Text -> Text -> ValueType -> NonEmpty Text
numOnlyErr op name vt =
  "Оператор «" <> op <> "» применим только к числовым полям."
    :| ["Поле «" <> name <> "» имеет " <> valueTypeDesc vt <> "."]

dateOnlyErr :: Text -> ValueType -> NonEmpty Text
dateOnlyErr name vt =
  "Сравнение «за … дней» применимо только к датовым полям."
    :| ["Поле «" <> name <> "» имеет " <> valueTypeDesc vt <> "."]

presenceErr :: Text -> Text -> NonEmpty Text
presenceErr op name =
  "Оператор «" <> op <> "» применим не ко всем полям."
    :| ["Поле «" <> name <> "» не поддерживает проверку наличия."]

operandErr :: Text -> Text -> Text -> NonEmpty Text
operandErr op expectedKind got =
  ("Оператор «" <> op <> "» ожидает " <> expectedKind <> " значение, получено «" <> got <> "».")
    :| []

valueMismatchErr :: Text -> Text -> ValueType -> NonEmpty Text
valueMismatchErr got name expected =
  ( "Значение «"
      <> got
      <> "» не соответствует типу поля «"
      <> name
      <> "»: ожидается "
      <> valueTypeExpect expected
      <> "."
  )
    :| []

dateEqErr :: Text -> NonEmpty Text
dateEqErr name =
  "Для датовых полей поддерживается только сравнение «за … дней»."
    :| ["Поле «" <> name <> "» имеет тип даты."]

daysErr :: Integer -> NonEmpty Text
daysErr n =
  ("Число дней должно быть положительным, получено " <> tshow n <> ".") :| []
  where
    tshow :: Integer -> Text
    tshow = T.pack . show

rangeErr :: Integer -> Integer -> NonEmpty Text
rangeErr lo hi =
  ( "Нижняя граница диапазона не может быть больше верхней: "
      <> tshow lo
      <> " > "
      <> tshow hi
      <> "."
  )
    :| []
  where
    tshow :: Integer -> Text
    tshow = T.pack . show
