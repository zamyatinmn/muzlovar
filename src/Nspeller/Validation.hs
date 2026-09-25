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
-- * недопустимые поля сортировки;
-- * логические противоречия условий (непротиворечивость) — см.
--   'semanticErrors': невыполнимые сочетания ограничений одного
--   поля внутри групп «все», включая вложенные группы;
-- * выход значений за известные границы поля и невыполнимые
--   диапазоны (см. 'domainError');
-- * предупреждения для валидного файла — 'semanticWarnings':
--   избыточные/дублирующиеся условия и условия, покрывающие весь
--   допустимый диапазон поля. Предупреждения возвращаются отдельным
--   списком и никогда не делают файл невалидным.
--
-- Все ошибки файла собираются целиком (не только первая), после чего
-- строится типобезопасный 'ValidPlaylist'.
module Nspeller.Validation
  ( validatePlaylist
  , validatePlaylistWithWarnings
  ) where

import Control.Monad (foldM, guard)
import Data.List (groupBy, maximumBy, minimumBy, nub, sortOn, tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, mapMaybe)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Nspeller.Ast
import Nspeller.Fields (FieldKind (..), fieldByCapability, fieldKind)

------------------------------------------------------------------------------
-- Точка входа
------------------------------------------------------------------------------

-- | Проверяет разобранный файл и строит валидированный AST.
--
-- Инвариант: 'Left' всегда содержит хотя бы одну ошибку.
-- Предупреждения ('validatePlaylistWithWarnings') здесь отбрасываются.
validatePlaylist :: FilePath -> Text -> ParsedFile -> Either [CompileError] ValidPlaylist
validatePlaylist fp src parsed = fmap fst (validatePlaylistWithTree fp src parsed)

-- | Как 'validatePlaylist', но возвращает также и предупреждения
-- (пустой список, если их нет). Предупреждения НЕ меняют результат
-- проверки: невалидный файл остаётся 'Left' без предупреждений.
validatePlaylistWithWarnings ::
  FilePath ->
  Text ->
  ParsedFile ->
  Either [CompileError] (ValidPlaylist, [CompileError])
validatePlaylistWithWarnings fp src parsed = do
  (vp, tree) <- validatePlaylistWithTree fp src parsed
  pure (vp, semanticWarnings fp src tree)

-- | Внутренний конвейер: валидированный AST плюс позиционированное
-- дерево условий (нужно 'semanticWarnings' для позиций предупреждений).
validatePlaylistWithTree ::
  FilePath ->
  Text ->
  ParsedFile ->
  Either [CompileError] (ValidPlaylist, PGroup)
validatePlaylistWithTree fp src (ParsedFile stmts) =
  let nameR = reqSection "подборка" projName
      descR = optSection "описание" projDesc
      publicR = optSection "публичная" projPublic
      whereR = reqSection "где" projWhere
      sortR = optSection "порядок" projSort
      limitR = optSection "лимит" projLimit

      groupR = case whereR of
        Left errs -> Left errs
        Right lg ->
          let LogicGroup kind items = locValue lg
              GroupBuild gErrs pItems = validateGroup fp src items
              root = PGroup kind pItems
              allGErrs = gErrs ++ semanticErrors fp src root
           in if null allGErrs
                then Right (toValidGroup root, root)
                else Left allGErrs

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
        (Right name, Right desc, Right pub, Right (grp, tree), Right srt, Right lim)
          | null allErrs ->
              Right
                ( ValidPlaylist
                    { vpName = locValue name
                    , vpDescription = locValue <$> desc
                    , vpPublic = isJust pub
                    , vpRoot = grp
                    , vpSort = srt
                    , vpLimit = lim
                    }
                , tree
                )
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

-- | Элемент позиционированного дерева условий: валидированное
-- условие с позицией в исходнике либо вложенная группа.
data PItem
  = PICond (Located ValidCond)
  | PIGroup PGroup

-- | Позиционированная группа условий. Строится во время валидации и
-- используется для анализа непротиворечивости ('semanticErrors');
-- типобезопасный 'ValidGroup' из неё получает 'toValidGroup'.
data PGroup = PGroup GroupKind [PItem]

-- | Результат разбора группы: ошибки невалидных элементов и дерево
-- из успешно проверенных условий. Невалидные элементы в дерево не
-- попадают — их ошибки уже собраны, а пропуск только ослабляет
-- проверку непротиворечивости (ложных срабатываний не появляется).
data GroupBuild = GroupBuild
  { gbErrs :: [CompileError]
  , gbItems :: [PItem]
  }

-- | Результат разбора одного элемента (аналог 'GroupBuild').
data ItemBuild = ItemBuild
  { ibErrs :: [CompileError]
  , ibItems :: [PItem]
  }

validateGroup :: FilePath -> Text -> [Located CondItem] -> GroupBuild
validateGroup fp src items =
  let results = map (validateItem fp src) items
   in GroupBuild (concatMap ibErrs results) (concatMap ibItems results)

validateItem :: FilePath -> Text -> Located CondItem -> ItemBuild
validateItem fp src li = case locValue li of
  CICond raw -> case validateCond fp src (Located (locStart li) (locEnd li) raw) of
    Left errs -> ItemBuild errs []
    Right c -> ItemBuild [] [PICond (Located (locStart li) (locEnd li) c)]
  CIGroup grp ->
    let LogicGroup kind items = grp
        gb = validateGroup fp src items
     in ItemBuild (gbErrs gb) [PIGroup (PGroup kind (gbItems gb))]

-- | Перевод позиционированного дерева в типобезопасный AST.
toValidGroup :: PGroup -> ValidGroup
toValidGroup (PGroup kind items) = ValidGroup kind (map toItem items)
  where
    toItem = \case
      PICond c -> VIC (locValue c)
      PIGroup g -> VIG (toValidGroup g)

------------------------------------------------------------------------------
-- Условия
------------------------------------------------------------------------------

validateCond :: FilePath -> Text -> Located RawCond -> Either [CompileError] ValidCond
validateCond fp src lc = case validateCond1 (locValue lc) of
  Left msgs -> Left [errorAt fp src lc msgs]
  Right valid -> case domainError valid of
    Nothing -> Right valid
    Just msgs -> Left [errorAt fp src lc msgs]

-- | Проверка значения по известным границам поля
-- ('fieldNumConstraints'):
--
-- * значение сравнения или граница «между» вне границ поля — ошибка;
-- * условие, не допускающее ни одного допустимого значения
--   (невыполнимый диапазон), — тоже ошибка.
--
-- Для полей с неизвестной областью значений ('Nothing') проверки
-- не выполняются.
domainError :: ValidCond -> Maybe (NonEmpty Text)
domainError = \case
  VNumber f op n -> case fieldNumConstraints f of
    Nothing -> Nothing
    Just nc
      | outsideDomain nc n -> Just (outOfRangeErr (fieldDslName f) nc n)
      | numEmpty nc op n -> Just (noValuesErr (fieldDslName f) nc)
      | otherwise -> Nothing
  VBetween f lo hi -> case fieldNumConstraints f of
    Nothing -> Nothing
    Just nc
      | maybe False (hi <) (ncMin nc) || maybe False (lo >) (ncMax nc) ->
          Just (rangeDisjointErr (fieldDslName f) nc lo hi)
      | maybe False (lo <) (ncMin nc) || maybe False (hi >) (ncMax nc) ->
          Just (rangeOutsideErr (fieldDslName f) nc lo hi)
      | otherwise -> Nothing
  _ -> Nothing
  where
    outsideDomain nc n =
      maybe False (n <) (ncMin nc) || maybe False (n >) (ncMax nc)

    -- Единственное условие, исключающее целый домен: строгое
    -- сравнение, упирающееся в границу (@прослушиваний < 0@,
    -- @оценка > 5@). Остальные операторы при значении внутри
    -- границ допускают хотя бы одно значение.
    numEmpty nc op n = case op of
      NLt -> maybe False (n <=) (ncMin nc)
      NGt -> maybe False (n >=) (ncMax nc)
      _ -> False

-- | Проверка одного условия без позиции: позицию добавит 'validateCond'.
validateCond1 :: RawCond -> Either (NonEmpty Text) ValidCond
validateCond1 = \case
  -- Вид поля ('fieldKind') выбирает допустимую сборку 'ValidCond':
  -- записи реестра в этой точке не перечисляются.
  RBare name -> withField name $ \fld -> case fieldKind fld of
    KindBool -> Right (VBool fld True)
    _ -> Left (bareErr name (fieldValueType fld))
  RBin name op val -> withField name $ \fld -> validateBin name fld op val
  RBetween name lo hi -> withField name $ \fld -> case fieldKind fld of
    KindNumber -> betweenNum name lo hi fld
    _ -> Left (numOnlyErr "между" name (fieldValueType fld))
  RDateBetween name lo hi -> withField name $ \fld -> case fieldKind fld of
    KindDate -> dateRange lo hi fld
    _ -> Left (dateCmpOnlyErr "между" name (fieldValueType fld))
  RPresence name pop -> withField name $ \fld ->
    if fieldPresence fld
      then Right (VPresence (SomeField fld) pop)
      else Left (presenceErr (presenceOpDesc pop) name)
  RRelative name days -> withField name $ \fld -> case fieldKind fld of
    KindDate
      | days > 0 -> Right (VRelative fld InTheLast days)
      | otherwise -> Left (daysErr days)
    _ -> Left (dateOnlyErr name (fieldValueType fld))
  RNotRelative name days -> withField name $ \fld -> case fieldKind fld of
    KindDate
      | days > 0 -> Right (VRelative fld NotInTheLast days)
      | otherwise -> Left (daysErr days)
    _ -> Left (notSinceErr name (fieldValueType fld))
  RNotPlayed days
    | days > 0 -> case notPlayedField of
        Just fld -> Right (VRelative fld NotInTheLast days)
        -- Недостижимо: реестр обязан содержать датовое поле с
        -- возможностью 'CapNotPlayed' (инвариант проверяется
        -- тестами реестра). Защита нужна лишь для полноты.
        Nothing -> Left internalErr
    | otherwise -> Left (daysErr days)
  RPlaylist m ref@(PlaylistRef kind value)
    | T.null (T.strip value) -> Left (playlistRefEmptyErr kind)
    | otherwise -> Right (VPlaylist m ref)
  where
    withField ::
      Text ->
      (forall a. FieldRef a -> Either (NonEmpty Text) ValidCond) ->
      Either (NonEmpty Text) ValidCond
    withField name k = case fieldByName name of
      Nothing -> Left (unknownFieldErr name)
      Just (SomeField fld) -> k fld

    -- Цель сахара «не звучало N дней»: поле с возможностью
    -- 'CapNotPlayed' и датовым видом берётся из реестра, конкретное
    -- поле здесь не называется. Совпадение по виду уточняет индекс
    -- ссылки до 'Day'.
    notPlayedField :: Maybe (FieldRef Day)
    notPlayedField = case fieldByCapability CapNotPlayed of
      Just (SomeField fld) -> case fieldKind fld of
        KindDate -> Just fld
        _ -> Nothing
      Nothing -> Nothing

    betweenNum ::
      Text ->
      Scientific ->
      Scientific ->
      FieldRef Scientific ->
      Either (NonEmpty Text) ValidCond
    betweenNum name' lo hi f
      | fieldIsIntegral f && not (isIntegralNumber lo) =
          Left (fractionalErr name' (formatNumber lo))
      | fieldIsIntegral f && not (isIntegralNumber hi) =
          Left (fractionalErr name' (formatNumber hi))
      | lo <= hi = Right (VBetween f lo hi)
      | otherwise = Left (rangeErr lo hi)

    dateRange ::
      Day -> Day -> FieldRef Day -> Either (NonEmpty Text) ValidCond
    dateRange lo hi f
      | lo <= hi = Right (VDateRange f lo hi)
      | otherwise = Left (dateRangeErr lo hi)

-- | Проверка бинарного условия: вид поля ('fieldKind') выбирает
-- разбор операнда и набор допустимых операторов — capabilities из
-- спецификации поля (их перечень в валидации: текстовые, числовые,
-- булевы, датовые), поэтому записи реестра здесь не перечисляются.
validateBin ::
  Text ->
  FieldRef a ->
  RawOp ->
  RawValue ->
  Either (NonEmpty Text) ValidCond
validateBin name fld op val = case fieldKind fld of
  KindText -> binT fld
  KindNumber -> binN fld
  KindBool -> binB fld
  KindDate -> binD fld
  -- Недостижимо: псевдополе «подборка» возможности 'CapStatic' не
  -- имеет и через 'fieldByName' не разрешается, поэтому ссылки с
  -- этим видом в условии не существует. Защита нужна лишь для
  -- полноты.
  KindPlaylistRef -> Left internalErr
  where
    -- Совпадение по виду уточнило индекс поля, поэтому сборка
    -- 'ValidCond' в ветках типобезопасна.

    binT :: FieldRef Text -> Either (NonEmpty Text) ValidCond
    binT f = case op of
      OpEq -> tEq False
      OpNe -> tEq True
      OpContains -> tLike TContains
      OpNotContains -> tLike TNotContains
      OpStartsWith -> tLike TStartsWith
      OpEndsWith -> tLike TEndsWith
      -- Неоператоры текста: числовое сравнение требует числового
      -- поля, границы даты — датового (см. 'opKindErr').
      unsupported -> Left (opKindErr unsupported name TextType)
      where
        -- Для полей с закрытым набором (enum) равенство и
        -- неравенство допускают только известные значения; пустая
        -- строка разрешена лишь тогда, когда входит в набор сама
        -- («Не определено» у explicitstatus).
        tEq negated = case val of
          RVText t -> case fieldEnum f of
            Just variants
              | t `notElem` map evValue variants ->
                  Left (enumErr name variants t)
              | otherwise ->
                  Right (VText f (if negated then TNe else TEq) t)
            Nothing
              | negated -> Right (VText f TNe t)
              | T.null t -> Left (emptyEqErr name)
              | otherwise -> Right (VText f TEq t)
          _ -> Left (valueMismatchErr (rawValueText val) name TextType)

        tLike top = case val of
          RVText t -> Right (VText f top t)
          _ -> Left (operandErr (rawOpDesc op) "текстовое" (rawValueText val))

    binN :: FieldRef Scientific -> Either (NonEmpty Text) ValidCond
    binN f = case op of
      OpEq -> nEq False
      OpNe -> nEq True
      OpGt -> nCmp NGt
      OpGe -> nCmp NGe
      OpLt -> nCmp NLt
      OpLe -> nCmp NLe
      -- Неоператоры числа: строковые и границы даты (см. 'opKindErr').
      unsupported -> Left (opKindErr unsupported name NumberType)
      where
        nEq negated = case val of
          RVNumber n
            | fractional f n -> Left (fractionalErr name (formatNumber n))
            | otherwise -> Right (VNumber f (if negated then NNe else NEq) n)
          _ -> Left (valueMismatchErr (rawValueText val) name NumberType)

        nCmp ctor = case val of
          RVNumber n
            | fractional f n -> Left (fractionalErr name (formatNumber n))
            | otherwise -> Right (VNumber f ctor n)
          _ -> Left (operandErr (rawOpDesc op) "числовое" (rawValueText val))

    -- Дробный операнд целочисленного поля (см. 'fieldIsIntegral').
    fractional :: FieldRef a -> Scientific -> Bool
    fractional f' n = fieldIsIntegral f' && not (isIntegralNumber n)

    binB :: FieldRef Bool -> Either (NonEmpty Text) ValidCond
    binB f = case op of
      OpEq -> bEq False
      OpNe -> bEq True
      -- Неоператоры флага: числовое сравнение, границы даты и
      -- строковые (см. 'opKindErr').
      unsupported -> Left (opKindErr unsupported name BoolType)
      where
        bEq negated = case val of
          RVBool b -> Right (VBool f (if negated then not b else b))
          _ -> Left (valueMismatchErr (rawValueText val) name BoolType)

    -- Сравнение датового поля: значение обязано быть датой
    -- (иначе — «ожидается дата в формате ГГГГ-ММ-ДД»), оператор —
    -- одним из сравнительных. Дата в кавычках разбирается 'valueP'
    -- как строка (иначе строковые поля с датоподобными значениями
    -- ломались бы) — здесь она принимается, если является
    -- календарной датой.
    binD :: FieldRef Day -> Either (NonEmpty Text) ValidCond
    binD f = case op of
      OpEq -> dCmp DEq
      OpNe -> dCmp DNe
      OpGt -> dCmp DGt
      OpGe -> dCmp DGe
      OpLt -> dCmp DLt
      OpLe -> dCmp DLe
      OpBefore -> dCmp DBefore
      OpAfter -> dCmp DAfter
      -- Строковые операторы датовым полям не применимы
      -- (см. 'opKindErr').
      unsupported -> Left (opKindErr unsupported name DateType)
      where
        dCmp :: DateOp -> Either (NonEmpty Text) ValidCond
        dCmp top = case dayOperand val of
          Just d -> Right (VDate f top d)
          Nothing -> Left (valueMismatchErr (rawValueText val) name DateType)

    -- Операнд даты: датовый литерал или строка, являющаяся
    -- календарной датой (см. комментарий к 'binD').
    dayOperand :: RawValue -> Maybe Day
    dayOperand = \case
      RVDate d -> Just d
      RVText t -> parseDay t
      _ -> Nothing

------------------------------------------------------------------------------
-- Семантика: непротиворечивость условий
------------------------------------------------------------------------------

-- | Проверяет дерево условий на гарантированные логические
-- противоречия и возвращает ошибки с позициями условий.
--
-- Анализ НЕ решает общую задачу выполнимости: условия помечаются
-- ошибочными только тогда, когда противоречие доказуемо
-- элементарно — по пересечению числовых диапазонов, несовместимым
-- равенствам, строковым ограничениям, булевым значениям и
-- проверкам наличия одного поля. Ветви «любое», где остаётся хоть
-- одна возможная комбинация, ошибкой не считаются: например,
-- @любое { год > 2020, год < 2000 }@ валидно, а @все { год > 2020,
-- год < 2000 }@ — нет.
--
-- Группы разворачиваются в DNF («миры»): «все» — декартово
-- произведение элементов, «любое» — объединение альтернатив.
-- Это позволяет доказывать противоречия и во вложенных группах
-- (в том числе когда конфликтующие условия лежат в разных
-- подгруппах), и не требует полноценного solver'а: при слишком
-- большом числе комбинаций проверка собственных миров группы
-- пропускается — это только уменьшает число находок и не создаёт
-- ложных срабатываний.
semanticErrors :: FilePath -> Text -> PGroup -> [CompileError]
semanticErrors fp src root =
  sortOn cePos
    [ errorAt fp src (Located s e ()) (candMsgs c)
    | c <- dedupCandidates (groupCandidates src root)
    , let (s, e) = candLoc c
    ]

-- | Одно условие, разложенное в требования к полю.
data Fact = Fact
  { factLoc :: (Int, Int)
  -- ^ Начало и конец условия в исходнике.
  , factField :: Text
  -- ^ Имя поля в записи DSL: ключ группировки и часть сообщений.
  , factProps :: FieldProps
  -- ^ Признаки поля (целочность, multivalue) — ими оперируют
  -- функции непротиворечивости.
  , factPrims :: [Prim]
  -- ^ Элементарные требования, которые накладывает условие.
  }
  deriving (Eq, Show)

-- | Признаки поля для анализа непротиворечивости.
data FieldProps = FieldProps
  { fpIntegral :: Bool
  -- ^ Домен целый: строгое сравнение ⇔ сдвиг границы на единицу
  -- ('fieldIsIntegral').
  , fpMultivalue :: Bool
  -- ^ Поле multivalue: два равенства описывают разные значения
  -- одного поля и не противоречат друг другу ('fieldMultivalue').
  }
  deriving (Eq, Show)

-- | Признаки поля из его спецификации.
fieldProps :: FieldRef a -> FieldProps
fieldProps f = FieldProps (fieldIsIntegral f) (fieldMultivalue f)

-- | Признаки поля валидированного условия.
condProps :: ValidCond -> FieldProps
condProps = \case
  VText f _ _ -> fieldProps f
  VNumber f _ _ -> fieldProps f
  VBetween f _ _ -> fieldProps f
  VBool f _ -> fieldProps f
  VRelative f _ _ -> fieldProps f
  VDate f _ _ -> fieldProps f
  VDateRange f _ _ -> fieldProps f
  -- Присутствие: примы только булевы, флаги не участвуют.
  VPresence _ _ -> FieldProps True False
  -- Членство в подборке: прима одна, домен не нужен.
  VPlaylist _ _ -> FieldProps True False

-- | Элементарное требование одного поля. Смешивать разные семьи
-- (числа и строки) в одном поле нельзя по построению: имя поля
-- однозначно определяет тип его значения.
data Prim
  = PLo Scientific Bool
  -- ^ Нижняя граница: значение и строгость (@x ≥ v@ / @x > v@).
  | PHi Scientific Bool
  -- ^ Верхняя граница: значение и строгость (@x ≤ v@ / @x < v@).
  | PEq Scientific
  -- ^ Равенство числу.
  | PNe Scientific
  -- ^ Неравенство числу.
  | PEqT Text
  -- ^ Равенство строке.
  | PNeT Text
  -- ^ Неравенство строке.
  | PContains Text
  -- ^ Содержит подстроку.
  | PNotContains Text
  -- ^ Не содержит подстроку.
  | PPrefix Text
  -- ^ Начинается с префикса.
  | PSuffix Text
  -- ^ Заканчивается на суффикс.
  | PBool Bool
  -- ^ Требуемое значение логического поля.
  | PPresence PresenceOp
  -- ^ Требование наличия значения поля.
  | PRelLo Integer Bool
  -- ^ Нижняя граница ОТНОСИТЕЛЬНОГО возраста события в днях
  -- (@за N дней@ / @не за N дней@) и строгость: возраст ≥ N / > N.
  | PRelHi Integer Bool
  -- ^ Верхняя граница относительного возраста: возраст ≤ N / < N.
  | PPlaylist PlaylistMembership PlaylistRef
  -- ^ Членство в подборке: направленность (в подборке / не в
  -- подборке) и сама ссылка (вид + значение).
  deriving (Eq, Show)

-- | Дата → её номер на оси МJD (Modified Julian Day: days since
-- 1858-11-17). Абсолютные даты живут на этой оси, «за N дней» — на
-- собственной относительной оси ('PRelLo'/'PRelHi'): оси не
-- смешиваются (см. 'combinedFeasible').
dayNum :: Day -> Scientific
dayNum d = fromInteger (diffDays d (fromGregorian 1858 11 17))

-- | Условие валидированного AST → разложенное требование.
factOf :: Located ValidCond -> Fact
factOf (Located s e c) = Fact (s, e) (condDslName c) (condProps c) (condPrims c)

-- | Имя поля условия в записи DSL.
condDslName :: ValidCond -> Text
condDslName = \case
  VText f _ _ -> fieldDslName f
  VNumber f _ _ -> fieldDslName f
  VBetween f _ _ -> fieldDslName f
  VBool f _ -> fieldDslName f
  VRelative f _ _ -> fieldDslName f
  VDate f _ _ -> fieldDslName f
  VDateRange f _ _ -> fieldDslName f
  VPresence (SomeField f) _ -> fieldDslName f
  VPlaylist _ _ -> playlistRefDslName

-- | Требования, которые накладывает условие на своё поле.
condPrims :: ValidCond -> [Prim]
condPrims = \case
  VText _ op val -> case op of
    TEq -> [PEqT val]
    TNe -> [PNeT val]
    TContains -> [PContains val]
    TNotContains -> [PNotContains val]
    TStartsWith -> [PPrefix val]
    TEndsWith -> [PSuffix val]
  VNumber _ op n -> case op of
    NEq -> [PEq n]
    NNe -> [PNe n]
    NGt -> [PLo n True]
    NGe -> [PLo n False]
    NLt -> [PHi n True]
    NLe -> [PHi n False]
  VBetween _ lo hi -> [PLo lo False, PHi hi False]
  VBool _ b -> [PBool b]
  VRelative _ op days -> case op of
    -- «за N дней» — событие произошло не позднее N дней назад;
    -- «не звучало N дней» / «не за N дней» — позднее N дней назад.
    -- Относительная ось (см. 'dayNum'): примы не смешиваются с
    -- абсолютными датами одного поля.
    InTheLast -> [PRelHi days False]
    NotInTheLast -> [PRelLo days True]
  VDate _ op d -> case op of
    DEq -> [PEq (dayNum d)]
    DNe -> [PNe (dayNum d)]
    DGt -> [PLo (dayNum d) True]
    DGe -> [PLo (dayNum d) False]
    DLt -> [PHi (dayNum d) True]
    DLe -> [PHi (dayNum d) False]
    DBefore -> [PHi (dayNum d) True]
    DAfter -> [PLo (dayNum d) True]
  VDateRange _ lo hi -> [PLo (dayNum lo) False, PHi (dayNum hi) False]
  VPresence _ op -> [PPresence op]
  VPlaylist m ref -> [PPlaylist m ref]

-- | Набор требований к одному полю доказуем невыполним.
--
-- Проверяются только очевидные гарантированные противоречия:
--
-- * числовые: пересечение границ с учётом целочности полей
--   (@год > 1980@ и @год < 1981@ не пересекаются ни на каком
--   целом числе), равенство вне границ или под запретом @!=@,
--   диапазон, целиком покрытый условиями @!=@;
-- * строковые: два разных @=@, @=@ с @!=@, значение, не проходящее
--   @содержит@ / @не содержит@ / @начинается с@ / @заканчивается
--   на@, несравнимые префиксы или суффиксы, @содержит@ вместе с
--   подстрокой из @не содержит@;
-- * логические: два взаимоисключающих значения одного поля;
-- * наличие: одновременно «присутствует» и «отсутствует», а также
--   «отсутствует» вместе с равенством конкретной строке.
--
-- 'FieldProps' поля (целочность, multivalue) определяют, как
-- трактуются границы и равенства: см. 'numericFeasible' и
-- 'textFeasible'.
combinedFeasible :: FieldProps -> [Prim] -> Bool
combinedFeasible props prims = numOk && relOk && textOk && boolOk && presenceOk && absenceOk && playlistOk
  where
    numOk = numericFeasible (fpIntegral props) [p | p <- prims, isNumPrim p]
    -- Относительные границы («за N дней») живут на своей оси и
    -- проверяются отдельно: абсолютные даты поля с ними не
    -- сравниваются.
    relOk = numericFeasible True (mapMaybe relPrim prims)
    textOk = textFeasible (fpMultivalue props) [p | p <- prims, isTextPrim p]
    boolOk = not (any (== PBool True) prims && any (== PBool False) prims)
    presenceOk =
      not (any (== PPresence Present) prims && any (== PPresence Absent) prims)
    absenceOk =
      not (any (== PPresence Absent) prims && any isTextEq prims)
    -- Членство в подборке: противоречие только у пары с ОДИНАКОВОЙ
    -- ссылкой и разной направленностью («в подборке X» + «не в
    -- подборке X»). Разные ссылки — независимые ограничения.
    playlistOk = not (playlistContradicts prims)

-- | Среди требований есть пара «в подборке X» + «не в подборке X»
-- одной и той же ссылки X.
playlistContradicts :: [Prim] -> Bool
playlistContradicts prims =
  or
    [ m1 /= m2 && r1 == r2
    | PPlaylist m1 r1 <- prims
    , PPlaylist m2 r2 <- prims
    ]

-- | Относительное требование → эквивалентная граница на оси дней
-- (для проверок, общих для границ): 'Nothing' — не относительное.
relPrim :: Prim -> Maybe Prim
relPrim = \case
  PRelLo d s -> Just (PLo (fromIntegral d) s)
  PRelHi d s -> Just (PHi (fromIntegral d) s)
  _ -> Nothing

-- | Числовое требование АБСОЛЮТНОЙ оси (граница, равенство,
-- неравенство). Относительные 'PRelLo'/'PRelHi' сюда не входят —
-- у них своя ось (см. 'relPrim').
isNumPrim :: Prim -> Bool
isNumPrim = \case
  PLo {} -> True
  PHi {} -> True
  PEq _ -> True
  PNe _ -> True
  _ -> False

-- | Строковое требование.
isTextPrim :: Prim -> Bool
isTextPrim = \case
  PEqT _ -> True
  PNeT _ -> True
  PContains _ -> True
  PNotContains _ -> True
  PPrefix _ -> True
  PSuffix _ -> True
  _ -> False

isTextEq :: Prim -> Bool
isTextEq (PEqT _) = True
isTextEq _ = False

-- | Числовые требования к одному полю невыполнимы.
--
-- 'Bool' — целочность домена поля ('fieldIsIntegral'). У целых
-- полей строгое сравнение эквивалентно сдвигу границы на единицу
-- (@x > 1980@ ⇔ @x ≥ 1981@), у дробных (@rgtrackgain@) строгоость
-- остаётся при границе, а бесконечный домен нельзя покрыть
-- конечным списком @!=@.
numericFeasible :: Bool -> [Prim] -> Bool
numericFeasible integral prims = case eqs of
  (n : rest) -> all (n ==) rest && within n && n `notElem` nes
  [] -> intervalOk
  where
    eqs = [n | PEq n <- prims]
    nes = [n | PNe n <- prims]
    -- Граница → (значение, строгость): для целых строгоость
    -- поглощается сдвигом на единицу.
    norm v strict = (v + strictAdj integral strict, if integral then False else strict)
    -- Верхняя граница при целочности сдвигается вниз: @x < 1981@
    -- ⇔ @x ≤ 1980@.
    normHigh v strict = (v - strictAdj integral strict, if integral then False else strict)
    lows = [norm v strict | PLo v strict <- prims]
    his = [normHigh v strict | PHi v strict <- prims]

    within n =
      all (\(v, s) -> if s then n > v else n >= v) lows
        && all (\(v, s) -> if s then n < v else n <= v) his

    loP = case lows of
      [] -> Nothing
      xs -> Just (maximumBy (comparing fst) xs)
    hiP = case his of
      [] -> Nothing
      xs -> Just (minimumBy (comparing fst) xs)

    intervalOk = case (loP, hiP) of
      (Just (l, _), Just (h, _))
        | l > h -> False
        -- Вырожденный интервал: строгая граница на значении точки
        -- делает его пустым, иначе остаётся одна точка.
        | l == h ->
            let lowStrict = any (\(v, s) -> v == l && s) lows
                highStrict = any (\(v, s) -> v == h && s) his
             in not (lowStrict || highStrict) && l `notElem` nes
        -- Дробный домен бесконечен: конечными @!=@ его не покрыть.
        | not integral -> True
        | otherwise ->
            let size = h - l + 1
                excluded = length (nub [n | n <- nes, n >= l, n <= h])
             in -- Диапазон невыполним, если каждый его элемент
                -- запрещён условиями @!=@ (в частности, точечный
                -- диапазон, равный запрещённому числу).
                size > fromIntegral excluded
      -- Одна из границ не задана: интервал бесконечен, а запрещённых
      -- чисел конечное число — покрыть его нельзя.
      _ -> True

-- | Строковые требования к одному полю невыполнимы.
--
-- 'Bool' — multivalue поле ('fieldMultivalue'): условия описывают
-- разные значения одного поля, поэтому равенства не сравниваются
-- между собой и с остальными текстовыми ограничениями.
textFeasible :: Bool -> [Prim] -> Bool
textFeasible multivalue prims = case eqs of
  (e : rest)
    | multivalue -> prefixesOk && suffixesOk && forcedMiss
    | otherwise -> all (e ==) rest && e `notElem` nes && matches e
  [] -> prefixesOk && suffixesOk && forcedMiss
  where
    eqs = [t | PEqT t <- prims]
    nes = [t | PNeT t <- prims]
    contains = [t | PContains t <- prims]
    notContains = [t | PNotContains t <- prims]
    prefixes = [t | PPrefix t <- prims]
    suffixes = [t | PSuffix t <- prims]

    -- Равное значение обязано удовлетворять каждому строковому
    -- ограничению поля.
    matches e =
      all (`T.isInfixOf` e) contains
        && all (\nc -> not (nc `T.isInfixOf` e)) notContains
        && all (`T.isPrefixOf` e) prefixes
        && all (`T.isSuffixOf` e) suffixes

    -- Несколько префиксов (суффиксов) совместимы тогда и только
    -- тогда, когда любой из них является префиксом (суффиксом)
    -- другого: иначе нет строки, начинающейся с обоих.
    prefixesOk = chain T.isPrefixOf prefixes
    suffixesOk = chain T.isSuffixOf suffixes
    chain rel xs = and [rel a b || rel b a | (a : rest) <- tails xs, b <- rest]

    -- Запрещённая подстрока, гарантированно входящая в любое
    -- значение, удовлетворяющее ограничению, делает набор
    -- невыполнимым (свидетель — сама ограничивающая строка).
    forcedMiss =
      and [not (nc `T.isInfixOf` c) | c <- contains, nc <- notContains]
        && and [not (nc `T.isInfixOf` p) | p <- prefixes, nc <- notContains]
        && and [not (nc `T.isInfixOf` s) | s <- suffixes, nc <- notContains]

-- | Мир — набор условий, которые должны выполняться вместе при одном
-- выборе каждой группы «любое».
type World = [Fact]

-- | Ограничение на число миров: защита от взрыва декартовых
-- произведений во вложенных группах.
maxWorlds :: Int
maxWorlds = 512

-- | Разворачивает группы в список миров. 'Nothing' — комбинаций
-- слишком много, собственные противоречия такой группы не
-- проверяются (её вложенные группы при этом проверяются отдельно).
--
-- В «все» неизвестная подгруппа опускается: миров остаётся меньше,
-- значит, проверяется меньше условий — ложных срабатываний это не
-- создаёт. В «любое» неизвестность наследуется: пропустить
-- альтернативу нельзя — остальные ветви могли оказаться
-- невыполнимыми только в комбинации с ней.
worldsOf :: PGroup -> Maybe [World]
worldsOf (PGroup kind items) = case kind of
  Any -> case traverse itemWorlds items of
    Nothing -> Nothing
    Just wss ->
      let ws = concat wss
       in if length ws > maxWorlds then Nothing else Just ws
  All -> foldM addWorlds [[]] (map itemWorlds items)
  where
    itemWorlds (PICond f) = Just [[factOf f]]
    itemWorlds (PIGroup g) = worldsOf g

    addWorlds acc Nothing = Just acc
    addWorlds acc (Just ws) =
      let prod = [w ++ w' | w <- acc, w' <- ws]
       in if length prod > maxWorlds then Nothing else Just prod

-- | Кандидаты-ошибки группы: сначала вложенные группы (глубже —
-- точнее, их сообщения сохраняются при дедупликации), затем
-- собственные миры группы.
groupCandidates :: Text -> PGroup -> [Candidate]
groupCandidates src g@(PGroup _ items) =
  concatMap childCandidates items ++ ownCandidates
  where
    childCandidates (PICond _) = []
    childCandidates (PIGroup child) = groupCandidates src child
    ownCandidates = case worldsOf g of
      Nothing -> []
      Just worlds -> concatMap (worldCandidates src) worlds

-- | Кандидаты одного мира: по одному на каждое поле с
-- доказанным противоречием.
worldCandidates :: Text -> World -> [Candidate]
worldCandidates src world =
  [ candidateOf src conflict
  | facts <- fieldGroups world
  , Just conflict <- [fieldConflict facts]
  ]

-- | Факты мира, сгруппированные по полю: противоречие возможно
-- только между ограничениями одного поля.
fieldGroups :: World -> [[Fact]]
fieldGroups world =
  groupBy (\a b -> factField a == factField b) (sortOn factField world)

-- | Доказанное противоречие одного поля в одном мире.
data Conflict
  = ConflictPair Fact Fact
  -- ^ Несовместимая пара: первый факт раньше в исходнике, второй —
  -- тот, на который указывает ошибка.
  | ConflictAll Fact [Fact]
  -- ^ Каждая пара по отдельности возможна, но вся совокупность —
  -- нет: первый факт — позиция ошибки, список — все факты по
  -- порядку следования в исходнике.

-- | Ищет противоречие среди условий одного поля мира: сначала
-- несовместимую пару (сообщение назовёт именно её) и только если
-- пар нет — всю совокупность условий. Признаки поля ('factProps')
-- берутся у фактов группы: группировка идёт по имени поля, все
-- факты списка описывают одно поле.
fieldConflict :: [Fact] -> Maybe Conflict
fieldConflict [] = Nothing
fieldConflict facts@(anchor : _) = case pairs of
  ((a, b) : _) -> Just (ConflictPair a b)
  [] ->
    let ascending = sortOn factLoc facts
     in case ascending of
          [] -> Nothing
          _ ->
            let report = maximumBy (comparing factLoc) ascending
             in if combinedFeasible (factProps anchor) (concatMap factPrims facts)
                  then Nothing
                  else Just (ConflictAll report ascending)
  where
    sorted = sortOn factLoc facts
    pairs =
      [ (a, b)
      | (a : rest) <- tails sorted
      , b <- rest
      , not (combinedFeasible (factProps a) (factPrims a ++ factPrims b))
      ]

-- | Найденная ошибка непротиворечивости: поле, позиция условия, на
-- которое указывает ошибка, и сообщение.
data Candidate = Candidate
  { candField :: Text
  , candLoc :: (Int, Int)
  , candMsgs :: NonEmpty Text
  }

-- | Сообщение об ошибке из доказанного противоречия.
candidateOf :: Text -> Conflict -> Candidate
candidateOf src = \case
  ConflictPair earlier later ->
    Candidate
      (factField later)
      (factLoc later)
      ( "Условие «"
          <> factSnippet src later
          <> "» противоречит условию «"
          <> factSnippet src earlier
          <> "»: эти условия не могут выполняться одновременно."
          :| []
      )
  ConflictAll report allFacts ->
    Candidate
      (factField report)
      (factLoc report)
      ( "Условия поля «"
          <> factField report
          <> "» не могут выполняться одновременно: "
          <> T.intercalate ", " ["«" <> factSnippet src f <> "»" | f <- allFacts]
          <> "."
          :| []
      )

-- | Текст условия для сообщения: фрагмент исходника по позиции
-- условия (многострочное условие сжимается до одной строки);
-- пустая позиция — условие без текста.
factSnippet :: Text -> Fact -> Text
factSnippet src f =
  let (s, e) = factLoc f
      raw = T.strip (T.take (max 0 (e - s)) (T.drop (max 0 s) src))
      oneLine = if T.any (== '\n') raw then T.unwords (T.words raw) else raw
   in if T.null oneLine then "…" else oneLine

-- | Убирает повторы: противоречие вложенной группы находится и в
-- родительской (там те же миры). Сообщение глубже расположенной
-- группы сохраняется — оно указывает на более точный фрагмент.
dedupCandidates :: [Candidate] -> [Candidate]
dedupCandidates = go []
  where
    go kept [] = reverse kept
    go kept (c : cs)
      | any (sameSpot c) kept = go kept cs
      | otherwise = go (c : kept) cs
    sameSpot a b = candField a == candField b && candLoc a == candLoc b

------------------------------------------------------------------------------
-- Семантика: предупреждения (никак не влияют на валидность)
------------------------------------------------------------------------------

-- | Предупреждения для валидного файла. Вычисляются только когда
-- ошибок нет ('Left' предупреждений не содержит) и никогда не
-- превращают 'Right' в 'Left'.
--
-- Два источника:
--
-- * избыточность и дубли — по тем же мирам (DNF), что и
--   'semanticErrors': условие избыточно, если оно уже выполняется
--   при выполнении остальных условий того же поля в том же мире.
--   Условия разных ветвей «любое» лежат в разных мирах и поэтому
--   никогда не считаются дублями друг друга. Взаимно вытекающие
--   пары (точные дубликаты) помечаются один раз — на более позднем
--   условии;
-- * условие, покрывающее весь допустимый диапазон поля, — оно не
--   отбрасывает ни одного значения (например, @оценка >= 0@).
semanticWarnings :: FilePath -> Text -> PGroup -> [CompileError]
semanticWarnings fp src root =
  sortOn cePos (map warningAt (dedupWarnings (worldWarnings ++ coverageWarnings)))
  where
    worldWarnings = concatMap worldWarns feasibleWorlds

    feasibleWorlds =
      filter feasible (maybe [] id (worldsOf root))
      where
        -- Миры с доказанным противоречием уже дали ошибку (или будут
        -- ею даны): предупреждения для них не считаются.
        feasible w = all feasibleGroup (fieldGroups w)

        feasibleGroup [] = True
        feasibleGroup facts@(f : _) =
          combinedFeasible (factProps f) (concatMap factPrims facts)

    worldWarns w = concatMap fieldWarns (fieldGroups w)
      where
        fieldWarns [] = []
        fieldWarns fs0@(anchor : _) =
          let props = factProps anchor
              fs = sortOn factLoc fs0
              indexed = zip [0 :: Int ..] fs
              primsAt i = factPrims (fs !! i)
              impliedByOthers (i, _) =
                impliesPrims props (concat [primsAt j | (j, _) <- indexed, j /= i]) (primsAt i)
              mutual i j =
                impliesPrims props (primsAt j) (primsAt i)
                  && impliesPrims props (primsAt i) (primsAt j)
              -- Взаимные пары (дубликаты) помечаются только на более
              -- позднем условии: остаётся максимальный индекс пары.
              dropped i = any (\j -> mutual i j) [j | (j, _) <- indexed, j > i]
              flagged = [(i, f) | iv@(i, f) <- indexed, impliedByOthers iv, not (dropped i)]
           in mapMaybe (flagWarning props fs indexed) flagged

        flagWarning props fs indexed (i, f) =
          case [e | (j, e) <- indexed, j < i, mutualPair props j i] of
            (e : _) ->
              Just
                ( Warning
                    "duplicate"
                    (factField f)
                    (factLoc f)
                    ( "Условие «"
                        <> factSnippet src f
                        <> "» дублирует условие «"
                        <> factSnippet src e
                        <> "»."
                        :| []
                    )
                )
            [] ->
              Just
                ( Warning
                    "redundant"
                    (factField f)
                    (factLoc f)
                    ( "Условие «"
                        <> factSnippet src f
                        <> "» избыточно: оно выполняется при выполнении остальных условий поля «"
                        <> factField f
                        <> "»."
                        :| []
                    )
                )
          where
            primsAt k = factPrims (fs !! k)
            mutualPair _ j k =
              impliesPrims props (primsAt k) (primsAt j)
                && impliesPrims props (primsAt j) (primsAt k)

    coverageWarnings = mapMaybe coverageWarn (allTreeFacts root)

    coverageWarn f = do
      nc <- factConstraints f
      guard (coversDomain (fpIntegral (factProps f)) nc (factPrims f))
      pure
        ( Warning
            "coverage"
            (factField f)
            (factLoc f)
            ( "Условие «"
                <> factSnippet src f
                <> "» покрывает весь допустимый диапазон поля «"
                <> factField f
                <> "» ("
                <> boundsText nc
                <> ") и не отбрасывает ни одного значения."
                :| []
            )
        )

    warningAt w =
      let (s, e) = warnLoc w
       in errorAt fp src (Located s e ()) (warnMsgs w)

-- | Предупреждение одного условия.
data Warning = Warning
  { warnTag :: Text
  -- ^ Тип предупреждения: ключ дедупликации между мирами.
  , warnField :: Text
  , warnLoc :: (Int, Int)
  , warnMsgs :: NonEmpty Text
  }

-- | Все факты дерева условий (включая вложенные группы) — для
-- проверки покрытия домена независимо от миров.
allTreeFacts :: PGroup -> [Fact]
allTreeFacts (PGroup _ items) = concatMap itemFacts items
  where
    itemFacts (PICond f) = [factOf f]
    itemFacts (PIGroup g) = allTreeFacts g

-- | Известные ограничения поля факта ('Nothing' — не число или
-- область значений неизвестна).
factConstraints :: Fact -> Maybe NumConstraints
factConstraints f = case fieldByName (factField f) of
  Just (SomeField fld) -> fieldNumConstraints fld
  Nothing -> Nothing

-- | Условие покрывает весь допустимый диапазон: допускает каждое
-- значение поля из его домена. Равенство и неравенство всегда
-- что-то отбрасывают (домен любого известного поля конечен или
-- не огранич сверху), поэтому не покрывают никогда.
--
-- 'Bool' — целочность домена ('fieldIsIntegral'): у дробных полей
-- строгое сравнение не сдвигает границу (см. 'strictAdj').
coversDomain :: Bool -> NumConstraints -> [Prim] -> Bool
coversDomain integral nc prims
  | any eqish prims = False
  | otherwise = lowerOk && upperOk
  where
    eqish = \case
      PEq _ -> True
      PNe _ -> True
      _ -> False
    lows = [v + strictAdj integral s | PLo v s <- prims]
    his = [v - strictAdj integral s | PHi v s <- prims]
    lowerOk = case (ncMin nc, lows) of
      (_, []) -> True
      (Nothing, _) -> False
      (Just dm, ls) -> maximum ls <= dm
    upperOk = case (ncMax nc, his) of
      (_, []) -> True
      (Nothing, _) -> False
      (Just dm, hs) -> minimum hs >= dm

-- | Сдвиг границы строгого сравнения при переходе к включительной
-- форме: для целых полей @x > v@ ⇔ @x ≥ v + 1@, дробный домен
-- строгоость не поглощает — остаётся @0@.
strictAdj :: Bool -> Bool -> Scientific
strictAdj integral strict = if integral && strict then 1 else 0

-- | Совокупность требований @others@ (остальные условия одного поля
-- одного мира) вытекает в целевые требования @target@.
--
-- 'FieldProps' поля задаёт целочность домена: у целых полей
-- @x > v@ эквивалентно @x ≥ v + 1@, у дробных граница работает
-- как есть.
--
-- Проверка консервативна: ложное «избыточное условие» невозможно
-- (требуется доказанное включение множеств допустимых значений),
-- пропуск — возможен.
impliesPrims :: FieldProps -> [Prim] -> [Prim] -> Bool
impliesPrims props others target = all implied target
  where
    integral = fpIntegral props

    implied p = case p of
      -- Границы: граница остальных сильнее целевой — строго за
      -- значением или совпадает с невыборограниченной.
      PLo v strict -> maybe False (\l -> l > v || (l == v && not strict)) (boundLow others)
      PHi v strict -> maybe False (\h -> h < v || (h == v && not strict)) (boundHigh others)
      -- x = n вытекает, если интервал остальных — точка n.
      PEq n -> boundLow others == Just n && boundHigh others == Just n
      -- x ≠ n вытекает, если n вне интервала остальных или
      -- прямо запрещён ими.
      PNe n -> has (\case PEq m -> m == n; _ -> False) || has (\case PNe m -> m == n; _ -> False)
        || not (inInterval n)
      PEqT t -> has (\case PEqT u -> u == t; _ -> False)
      PNeT t ->
        has (\case PNeT u -> u == t; _ -> False)
          || any (\case PEqT u -> u /= t; _ -> False) others
          || any (\case PPrefix pre -> not (pre `T.isPrefixOf` t); _ -> False) others
          || any (\case PSuffix suf -> not (suf `T.isSuffixOf` t); _ -> False) others
          || any (\case PContains c -> not (c `T.isInfixOf` t); _ -> False) others
          || any (\case PNotContains c -> c `T.isInfixOf` t; _ -> False) others
      -- Содержание вытекает, если подстрока входит в любую
      -- фиксируемую другими условиями часть значения.
      PContains c ->
        any (\case PEqT u -> c `T.isInfixOf` u; _ -> False) others
          || any (\case PContains c2 -> c `T.isInfixOf` c2; _ -> False) others
          || any (\case PPrefix pre -> c `T.isInfixOf` pre; _ -> False) others
          || any (\case PSuffix suf -> c `T.isInfixOf` suf; _ -> False) others
      PNotContains c ->
        has (\case PNotContains d -> d == c; _ -> False)
          || any (\case PEqT u -> not (c `T.isInfixOf` u); _ -> False) others
      PPrefix pre ->
        any (\case PPrefix p2 -> pre `T.isPrefixOf` p2; _ -> False) others
          || any (\case PEqT u -> pre `T.isPrefixOf` u; _ -> False) others
      PSuffix suf ->
        any (\case PSuffix s2 -> suf `T.isSuffixOf` s2; _ -> False) others
          || any (\case PEqT u -> suf `T.isSuffixOf` u; _ -> False) others
      PBool b -> has (\case PBool b2 -> b == b2; _ -> False)
      PPresence o -> has (\case PPresence o2 -> o == o2; _ -> False)
      -- Членство в подборке вытекает только из точно такого же
      -- требования (та же направленность и та же ссылка): другие
      -- ссылки и противоположная направленность ничего не дают.
      PPlaylist m r ->
        has (\case PPlaylist m2 r2 -> m == m2 && r == r2; _ -> False)
      -- Относительные границы сравниваются только с относительными
      -- же: абсолютные даты и «за N дней» живут на разных осях.
      PRelLo d strict ->
        maybe False (\l -> l > d || (l == d && not strict)) (relBoundLow others)
      PRelHi d strict ->
        maybe False (\h -> h < d || (h == d && not strict)) (relBoundHigh others)

    has f = any f others

    inInterval n =
      maybe True (n >=) (boundLow others) && maybe True (n <=) (boundHigh others)

    -- Нижняя (верхняя) граница интервала остальных с учётом
    -- строгоости и равенств; 'Nothing' — не ограничена.
    boundLow ps = case [v + strictAdj integral s | PLo v s <- ps] ++ [n | PEq n <- ps] of
      [] -> Nothing
      xs -> Just (maximum xs)
    boundHigh ps = case [v - strictAdj integral s | PHi v s <- ps] ++ [n | PEq n <- ps] of
      [] -> Nothing
      xs -> Just (minimum xs)

    -- Как 'boundLow'/'boundHigh', но только по относительным
    -- остальным: возраст в днях целый, поэтому строгость
    -- поглощается сдвигом на единицу (@за 30 дней@ ⇔ @возраст
    -- ≤ 30@, @не за 30 дней@ ⇔ @возраст ≥ 31@).
    relBoundLow ps = case [d + (if s then 1 else 0) | PRelLo d s <- ps] of
      [] -> Nothing
      xs -> Just (maximum xs)
    relBoundHigh ps = case [d - (if s then 1 else 0) | PRelHi d s <- ps] of
      [] -> Nothing
      xs -> Just (minimum xs)

-- | Убирает повторы предупреждений: одно и то же условие видно во
-- многих мирах (и во вложенных группах).
dedupWarnings :: [Warning] -> [Warning]
dedupWarnings = go []
  where
    go kept [] = reverse kept
    go kept (w : ws)
      | any (sameSpot w) kept = go kept ws
      | otherwise = go (w : kept) ws
    sameSpot a b =
      warnTag a == warnTag b && warnField a == warnField b && warnLoc a == warnLoc b

------------------------------------------------------------------------------
-- Сортировка
------------------------------------------------------------------------------

validateSort :: FilePath -> Text -> RawSort -> Either [CompileError] SortMode
validateSort fp src = \case
  SortRandom -> Right SortRandomMode
  SortSpec items -> SortBy <$> step (map (validateSortItem fp src) items)

-- | Элемент секции сортировки: поле и направление. Все поля DSL
-- сортируются (в том числе логические — @любимое@, @обложка@),
-- поэтому неизвестное имя — единственная ошибка здесь.
validateSortItem :: FilePath -> Text -> Located RawSortItem -> Either [CompileError] SortItem
validateSortItem fp src li = case locValue li of
  RawSortItem name dir ->
    case sortFieldByName name of
      Just sf -> Right (SortItem sf (toSortDir dir))
      Nothing ->
        Left [errorAt fp src li ("Неизвестное поле сортировки «" <> name <> "»." :| [])]
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

emptyEqErr :: Text -> NonEmpty Text
emptyEqErr name =
  "Оператор «=» требует непустое текстовое значение."
    :| ["Поле «" <> name <> "» сравнивается с пустой строкой."]

-- | Оператор «до»/«после» (или «между») у не-датового поля.
dateCmpOnlyErr :: Text -> Text -> ValueType -> NonEmpty Text
dateCmpOnlyErr op name vt =
  "Оператор «" <> op <> "» применим только к датовым полям."
    :| ["Поле «" <> name <> "» имеет " <> valueTypeDesc vt <> "."]

-- | Несовместимость оператора с видом поля: семейство оператора
-- выбирает формулировку — числовое сравнение ждёт числового поля,
-- границы «до»/«после» — датового, остальное (содержит и т. п.) —
-- текстового. Равенство и неравенство совместимы со всеми видами и
-- сюда не попадают.
opKindErr :: RawOp -> Text -> ValueType -> NonEmpty Text
opKindErr op name cat
  | op `elem` [OpGt, OpGe, OpLt, OpLe] = numOnlyErr (rawOpDesc op) name cat
  | op `elem` [OpBefore, OpAfter] = dateCmpOnlyErr (rawOpDesc op) name cat
  | otherwise = textOnlyErr (rawOpDesc op) name cat

-- | Внутренняя ошибка компиляции для недостижимой ветки (текст
-- совпадает с 'Nspeller.Ast.fileError' в сборщике секций).
internalErr :: NonEmpty Text
internalErr = "внутренняя ошибка компиляции" :| []

daysErr :: Integer -> NonEmpty Text
daysErr n =
  ("Число дней должно быть положительным, получено " <> tshow n <> ".") :| []
  where
    tshow :: Integer -> Text
    tshow = T.pack . show

rangeErr :: Scientific -> Scientific -> NonEmpty Text
rangeErr lo hi =
  ( "Нижняя граница диапазона не может быть больше верхней: "
      <> formatNumber lo
      <> " > "
      <> formatNumber hi
      <> "."
  )
    :| []

-- | Как 'rangeErr', но для диапазона абсолютных дат.
dateRangeErr :: Day -> Day -> NonEmpty Text
dateRangeErr lo hi =
  ( "Нижняя граница диапазона не может быть больше верхней: "
      <> formatDay lo
      <> " > "
      <> formatDay hi
      <> "."
  )
    :| []

-- | Текстовый перечень допустимых границ поля для сообщений
-- о выходе за границы.
boundsText :: NumConstraints -> Text
boundsText nc = case (ncMin nc, ncMax nc) of
  (Just lo, Just hi) -> "от " <> num lo <> " до " <> num hi
  (Just lo, Nothing) -> "не меньше " <> num lo
  (Nothing, Just hi) -> "не больше " <> num hi
  (Nothing, Nothing) -> "без ограничений"
  where
    num :: Scientific -> Text
    num = formatNumber

outOfRangeErr :: Text -> NumConstraints -> Scientific -> NonEmpty Text
outOfRangeErr name nc n =
  ( "Значение "
      <> formatNumber n
      <> " вне допустимого диапазона поля «"
      <> name
      <> "»: "
      <> boundsText nc
      <> "."
  )
    :| []

noValuesErr :: Text -> NumConstraints -> NonEmpty Text
noValuesErr name nc =
  ( "Условие не может выполняться: оно не допускает ни одного допустимого значения поля «"
      <> name
      <> "» (допустимо: "
      <> boundsText nc
      <> ")."
  )
    :| []

rangeDisjointErr :: Text -> NumConstraints -> Scientific -> Scientific -> NonEmpty Text
rangeDisjointErr name nc lo hi =
  ( "Диапазон "
      <> formatNumber lo
      <> "…"
      <> formatNumber hi
      <> " не пересекает допустимый диапазон поля «"
      <> name
      <> "»: "
      <> boundsText nc
      <> "."
  )
    :| []

rangeOutsideErr :: Text -> NumConstraints -> Scientific -> Scientific -> NonEmpty Text
rangeOutsideErr name nc lo hi =
  ( "Диапазон "
      <> formatNumber lo
      <> "…"
      <> formatNumber hi
      <> " выходит за допустимые границы поля «"
      <> name
      <> "»: "
      <> boundsText nc
      <> "."
  )
    :| []

-- | Дробный операнд целочисленного поля (см. 'fieldIsIntegral').
fractionalErr :: Text -> Text -> NonEmpty Text
fractionalErr name got =
  ("Значение " <> got <> " должно быть целым числом.")
    :| ["Поле «" <> name <> "» не поддерживает дробные значения."]

-- | Значение равенства/неравенства вне закрытого набора поля
-- ('fieldEnum'): перечисляет @значение (подпись)@.
enumErr :: Text -> [EnumVariant] -> Text -> NonEmpty Text
enumErr name variants got =
  ("Значение «" <> got <> "» не входит в допустимые значения поля «" <> name <> "».")
    :| [ "Допустимо: "
           <> T.intercalate
             ", "
             ["«" <> evValue v <> "» (" <> evLabel v <> ")" | v <- variants]
           <> "."
       ]

-- | Пустая ссылка на подборку (после обрезки пробелов).
playlistRefEmptyErr :: PlaylistRefKind -> NonEmpty Text
playlistRefEmptyErr RefId = "Идентификатор подборки не может быть пустым." :| []
playlistRefEmptyErr RefPath = "Путь к файлу подборки не может быть пустым." :| []

-- | «Не за … дней» у не-датового поля.
notSinceErr :: Text -> ValueType -> NonEmpty Text
notSinceErr name vt =
  "Сравнение «не за … дней» применимо только к датовым полям."
    :| ["Поле «" <> name <> "» имеет " <> valueTypeDesc vt <> "."]
