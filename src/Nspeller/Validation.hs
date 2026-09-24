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
--   поля внутри групп «все», включая вложенные группы.
--
-- Все ошибки файла собираются целиком (не только первая), после чего
-- строится типобезопасный 'ValidPlaylist'.
module Nspeller.Validation
  ( validatePlaylist
  ) where

import Control.Monad (foldM)
import Data.List (groupBy, maximumBy, nub, sortOn, tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Ord (comparing)
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
        Right lg ->
          let LogicGroup kind items = locValue lg
              GroupBuild gErrs pItems = validateGroup fp src items
              root = PGroup kind pItems
              allGErrs = gErrs ++ semanticErrors fp src root
           in if null allGErrs then Right (toValidGroup root) else Left allGErrs

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
          RVText t
            | negated -> Right (VText f TNe t)
            | T.null t -> Left (emptyEqErr name)
            | otherwise -> Right (VText f TEq t)
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
  , factPrims :: [Prim]
  -- ^ Элементарные требования, которые накладывает условие.
  }
  deriving (Eq, Show)

-- | Элементарное требование одного поля. Смешивать разные семьи
-- (числа и строки) в одном поле нельзя по построению: имя поля
-- однозначно определяет тип его значения.
data Prim
  = PLo Integer Bool
  -- ^ Нижняя граница: значение и строгость (@x ≥ v@ / @x > v@).
  | PHi Integer Bool
  -- ^ Верхняя граница: значение и строгость (@x ≤ v@ / @x < v@).
  | PEq Integer
  -- ^ Равенство числу.
  | PNe Integer
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
  deriving (Eq, Show)

-- | Условие валидированного AST → разложенное требование.
factOf :: Located ValidCond -> Fact
factOf (Located s e c) = Fact (s, e) (condDslName c) (condPrims c)

-- | Имя поля условия в записи DSL.
condDslName :: ValidCond -> Text
condDslName = \case
  VText f _ _ -> fieldDslName f
  VNumber f _ _ -> fieldDslName f
  VBetween f _ _ -> fieldDslName f
  VBool f _ -> fieldDslName f
  VRelative f _ _ -> fieldDslName f
  VPresence p _ -> presenceFieldDslName p

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
    NLt -> [PHi n True]
  VBetween _ lo hi -> [PLo lo False, PHi hi False]
  VBool _ b -> [PBool b]
  VRelative _ op days -> case op of
    -- «за N дней» — событие произошло не позднее N дней назад;
    -- «не звучало N дней» — позднее N дней назад.
    InTheLast -> [PHi days False]
    NotInTheLast -> [PLo days True]
  VPresence _ op -> [PPresence op]

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
combinedFeasible :: [Prim] -> Bool
combinedFeasible prims = numOk && textOk && boolOk && presenceOk && absenceOk
  where
    numOk = numericFeasible [p | p <- prims, isNumPrim p]
    textOk = textFeasible [p | p <- prims, isTextPrim p]
    boolOk = not (any (== PBool True) prims && any (== PBool False) prims)
    presenceOk =
      not (any (== PPresence Present) prims && any (== PPresence Absent) prims)
    absenceOk =
      not (any (== PPresence Absent) prims && any isTextEq prims)

-- | Числовое требование (граница, равенство, неравенство).
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

-- | Числовые требования к одному полю невыполнимы (поля целые).
numericFeasible :: [Prim] -> Bool
numericFeasible prims = case eqs of
  (n : rest) -> all (n ==) rest && within n && n `notElem` nes
  [] -> intervalOk
  where
    eqs = [n | PEq n <- prims]
    nes = [n | PNe n <- prims]
    -- Целочность: @x > v@ ⇔ @x ≥ v + 1@, @x < v@ ⇔ @x ≤ v - 1@.
    lows = [v + (if strict then 1 else 0) | PLo v strict <- prims]
    his = [v - (if strict then 1 else 0) | PHi v strict <- prims]
    lo = if null lows then Nothing else Just (maximum lows)
    hi = if null his then Nothing else Just (minimum his)
    within n = maybe True (n >=) lo && maybe True (n <=) hi
    intervalOk = case (lo, hi) of
      (Just l, Just h)
        | l > h -> False
        | otherwise ->
            let size = h - l + 1
                excluded = length (nub [n | n <- nes, n >= l, n <= h])
             in -- Диапазон невыполним, если каждый его элемент
                -- запрещён условиями @!=@ (в частности, точечный
                -- диапазон, равный запрещённому числу).
                size > toInteger excluded
      -- Одна из границ не задана: интервал бесконечен, а запрещённых
      -- чисел конечное число — покрыть его нельзя.
      _ -> True

-- | Строковые требования к одному полю невыполнимы.
textFeasible :: [Prim] -> Bool
textFeasible prims = case eqs of
  (e : rest) -> all (e ==) rest && e `notElem` nes && matches e
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
-- пар нет — всю совокупность условий.
fieldConflict :: [Fact] -> Maybe Conflict
fieldConflict facts = case pairs of
  ((a, b) : _) -> Just (ConflictPair a b)
  [] ->
    let ascending = sortOn factLoc facts
     in case ascending of
          [] -> Nothing
          _ ->
            let report = maximumBy (comparing factLoc) ascending
             in if combinedFeasible (concatMap factPrims facts)
                  then Nothing
                  else Just (ConflictAll report ascending)
  where
    sorted = sortOn factLoc facts
    pairs =
      [ (a, b)
      | (a : rest) <- tails sorted
      , b <- rest
      , not (combinedFeasible (factPrims a ++ factPrims b))
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

emptyEqErr :: Text -> NonEmpty Text
emptyEqErr name =
  "Оператор «=» требует непустое текстовое значение."
    :| ["Поле «" <> name <> "» сравнивается с пустой строкой."]

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
