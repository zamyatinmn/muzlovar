{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | DTO веб-редактора Muzlovar и их связь с ядром nspeller.
--
-- Модуль содержит:
--
-- * JSON-контракт дерева подборки ('PlaylistDto' и друзья);
-- * структурные проверки дерева ('dtoToParsed'): пустые группы,
--   чрезмерная вложенность, условия без значения;
-- * конвейер сохранения 'compilePlaylistDto': DTO → 'ParsedFile' →
--   канонический @.mix@ → 'parsePlaylist' → 'validatePlaylist' →
--   'encodeNsp'. Второго компилятора нет — используются только
--   существующие модули ядра, семантические проверки «поле ×
--   оператор × значение» остаются в 'Nspeller.Validation';
-- * обратный разбор @.nsp@ в DTO ('nspToDto'): неизвестные поля и
--   операторы превращаются в неизменяемые @raw@-узлы и не теряются;
-- * перевод валидированного AST в DTO ('validToDto').
module Nspeller.Muzlovar.Types
  ( -- * DTO
    PlaylistDto (..)
  , GroupDto (..)
  , ItemDto (..)
  , CondDto (..)
  , SortDto (..)
  , SortItemDto (..)

    -- * Ошибки API
  , ApiError (..)
  , DtoError (..)
  , apiError
  , compileErrorsToApi

    -- * Конвейер сохранения
  , Compiled (..)
  , compilePlaylistDto
  , compilePlaylistDtoWarnings

    -- * Обратные переводы
  , validToDto
  , nspToDto
  , dtoFromMix

    -- * Разбор DTO → ParsedFile
  , dtoToParsed

    -- * Позиции → путь элемента
  , offsetFromPos
  , pathAtOffset
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Scientific (Scientific)
import Data.Ord (comparing)
import Data.Ratio (denominator)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Nspeller.Ast
import Nspeller.Navidrome (encodeNsp, toNsp)
import Nspeller.Parser (parsePlaylist)
import Nspeller.Render (renderParsedFile)
import Nspeller.Validation (validatePlaylist, validatePlaylistWithWarnings)

------------------------------------------------------------------------------
-- Ошибки API
------------------------------------------------------------------------------

-- | Структурированная ошибка API: код, русское сообщение, путь до
-- элемента дерева (JSON-pointer вида @/root/items/0@) и позиция в
-- каноническом @.mix@ (для ошибок разбора и валидации).
data ApiError = ApiError
  { aeCode :: Text
  , aeMessage :: Text
  , aePath :: Maybe Text
  , aeLine :: Maybe Int
  , aeColumn :: Maybe Int
  }
  deriving (Eq, Show)

instance ToJSON ApiError where
  toJSON e =
    object
      [ "code" .= aeCode e
      , "message" .= aeMessage e
      , "path" .= aePath e
      , "line" .= aeLine e
      , "column" .= aeColumn e
      ]

-- | Ошибка без позиции и пути.
apiError :: Text -> Text -> ApiError
apiError code msg = ApiError code msg Nothing Nothing Nothing

-- | Ошибка структуры DTO (с путём до элемента).
data DtoError = DtoError
  { dePath :: Text
  , deMessage :: Text
  }
  deriving (Eq, Show)

dtoErrorsToApi :: [DtoError] -> [ApiError]
dtoErrorsToApi es =
  [ApiError "invalid_tree" (deMessage e) (Just (dePath e)) Nothing Nothing | e <- es]

------------------------------------------------------------------------------
-- DTO
------------------------------------------------------------------------------

-- | Полная подборка.
data PlaylistDto = PlaylistDto
  { pdName :: Text
  , pdDescription :: Maybe Text
  , pdPublic :: Bool
  , pdRoot :: GroupDto
  , pdSort :: Maybe SortDto
  , pdLimit :: Maybe Integer
  }
  deriving (Eq, Show)

-- | Логическая группа: @all@ (ВСЕ) или @any@ (ЛЮБОЕ).
data GroupDto = GroupDto
  { gdKind :: Text
  , gdItems :: [ItemDto]
  }
  deriving (Eq, Show)

-- | Элемент дерева: условие, вложенная группа или неизменяемый узел
-- из внешнего @.nsp@.
data ItemDto
  = ItemCond CondDto
  | ItemGroup GroupDto
  | ItemRaw Value
  deriving (Eq, Show)

-- | Условие. 'cdValue' — JSON-значение операнда: строка/число/bool
-- для бинарных операторов, @[lo, hi]@ для @between@, число дней для
-- относительных дат; 'Nothing' для @bare@/@isMissing@/@isPresent@.
data CondDto = CondDto
  { cdField :: Text
  , cdOp :: Text
  , cdValue :: Maybe Value
  }
  deriving (Eq, Show)

-- | Режим сортировки: случайный, список полей или сырой текст из
-- внешнего файла (только чтение).
data SortDto
  = SortRandomDto
  | SortFieldsDto [SortItemDto]
  | SortRawDto Text
  deriving (Eq, Show)

-- | Элемент сортировки: поле (имя из DSL) и направление.
data SortItemDto = SortItemDto
  { siField :: Text
  , siDir :: Text
  }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- JSON: DTO
------------------------------------------------------------------------------

instance ToJSON PlaylistDto where
  toJSON p =
    object
      [ "name" .= pdName p
      , "description" .= pdDescription p
      , "public" .= pdPublic p
      , "root" .= pdRoot p
      , "sort" .= pdSort p
      , "limit" .= pdLimit p
      ]

instance FromJSON PlaylistDto where
  parseJSON = withObject "PlaylistDto" $ \o ->
    PlaylistDto
      <$> o .: "name"
      <*> o .:? "description"
      <*> (fromMaybe False <$> o .:? "public")
      <*> o .: "root"
      <*> o .:? "sort"
      <*> o .:? "limit"

instance ToJSON GroupDto where
  toJSON g = object ["kind" .= gdKind g, "items" .= gdItems g]

instance FromJSON GroupDto where
  parseJSON = withObject "GroupDto" $ \o ->
    GroupDto <$> o .: "kind" <*> o .: "items"

instance ToJSON ItemDto where
  toJSON = \case
    ItemCond c ->
      object
        [ "type" .= ("cond" :: Text)
        , "field" .= cdField c
        , "op" .= cdOp c
        , "value" .= cdValue c
        ]
    ItemGroup g ->
      object ["type" .= ("group" :: Text), "kind" .= gdKind g, "items" .= gdItems g]
    ItemRaw v -> object ["type" .= ("raw" :: Text), "raw" .= v]

instance FromJSON ItemDto where
  parseJSON = withObject "ItemDto" $ \o -> do
    t <- o .: "type" :: Parser Text
    case t of
      "cond" -> ItemCond <$> (CondDto <$> o .: "field" <*> o .: "op" <*> o .:? "value")
      "group" -> ItemGroup <$> (GroupDto <$> o .: "kind" <*> o .: "items")
      "raw" -> ItemRaw <$> o .: "raw"
      other -> fail ("неизвестный тип элемента: " ++ show other)

instance ToJSON CondDto where
  toJSON c = object ["field" .= cdField c, "op" .= cdOp c, "value" .= cdValue c]

instance ToJSON SortDto where
  toJSON = \case
    SortRandomDto -> object ["kind" .= ("random" :: Text)]
    SortFieldsDto items -> object ["kind" .= ("fields" :: Text), "items" .= items]
    SortRawDto t -> object ["kind" .= ("raw" :: Text), "text" .= t]

instance FromJSON SortDto where
  parseJSON = withObject "SortDto" $ \o -> do
    k <- o .: "kind" :: Parser Text
    case k of
      "random" -> pure SortRandomDto
      "fields" -> SortFieldsDto <$> o .: "items"
      "raw" -> SortRawDto <$> o .: "text"
      other -> fail ("неизвестный режим сортировки: " ++ show other)

instance ToJSON SortItemDto where
  toJSON s = object ["field" .= siField s, "dir" .= siDir s]

instance FromJSON SortItemDto where
  parseJSON = withObject "SortItemDto" $ \o ->
    SortItemDto <$> o .: "field" <*> o .: "dir"

------------------------------------------------------------------------------
-- DTO → ParsedFile (структурные проверки)
------------------------------------------------------------------------------

-- | Максимальная глубина вложенности групп: защита от чрезмерно
-- глубокой рекурсии при разборе тела запроса.
maxDepth :: Int
maxDepth = 64

-- | Разбор DTO в разобранный AST с фиктивными позициями.
--
-- Выполняет только структурные проверки: непустое название, непустые
-- группы, допустимую глубину, наличие значений у условий, известные
-- типы элементов и режимы сортировки. Имена полей, операторы и типы
-- значений остаются на 'validatePlaylist'.
dtoToParsed :: PlaylistDto -> Either [DtoError] ParsedFile
dtoToParsed dto = do
  name <- checkName (pdName dto)
  checkDepth (pdRoot dto)
  root <- groupToLogic "/root" (pdRoot dto)
  sortStmts <- sortToStmts (pdSort dto)
  let stmts =
        [SName name]
          <> maybe [] (\d -> [SDescription d]) (pdDescription dto)
          <> [SPublic | pdPublic dto]
          <> [SWhere root]
          <> sortStmts
          <> maybe [] (\n -> [SLimit n]) (pdLimit dto)
  pure (ParsedFile (map (Located 0 0) stmts))
  where
    checkName n
      | T.null (T.strip n) = Left [DtoError "/name" "Название подборки не может быть пустым."]
      | otherwise = Right n

    checkDepth g
      | nesting g >= maxDepth =
          Left
            [ DtoError
                "/root"
                ("Слишком глубокая вложенность групп (максимум " <> tshow maxDepth <> ").")
            ]
      | otherwise = Right ()

    nesting g = maximum (0 : [1 + nesting gg | ItemGroup gg <- gdItems g])

    sortToStmts = \case
      Nothing -> Right []
      Just SortRandomDto -> Right [SSort SortRandom]
      Just (SortRawDto t) ->
        Left [DtoError "/sort" ("Режим сортировки из внешнего файла не поддерживается: " <> t)]
      Just (SortFieldsDto []) ->
        Left [DtoError "/sort" "Список полей сортировки не может быть пустым."]
      Just (SortFieldsDto items) ->
        (\is -> [SSort (SortSpec is)]) <$> traverse sortItem items

    sortItem s = case sortFieldByName (siField s) of
      Nothing -> Left [DtoError "/sort" ("Неизвестное поле сортировки «" <> siField s <> "».")]
      Just _ -> case siDir s of
        "asc" -> Right (Located 0 0 (RawSortItem (siField s) Ascending))
        "desc" -> Right (Located 0 0 (RawSortItem (siField s) Descending))
        d -> Left [DtoError "/sort" ("Неизвестное направление сортировки «" <> d <> "».")]

    groupToLogic :: Text -> GroupDto -> Either [DtoError] LogicGroup
    groupToLogic path g = case gdKind g of
      "all" -> go All
      "any" -> go Any
      k -> Left [DtoError path ("Неизвестная логика группы «" <> k <> "».")]
      where
        go kind = case gdItems g of
          [] -> Left [DtoError path "Группа условий не может быть пустой."]
          items ->
            LogicGroup kind
              <$> sequenceA
                ( zipWith
                    (\j -> itemToCond (path <> "/items/" <> tshow j))
                    [0 :: Int ..]
                    items
                )

    itemToCond :: Text -> ItemDto -> Either [DtoError] (Located CondItem)
    itemToCond path = \case
      ItemRaw _ ->
        Left [DtoError path "Элемент из внешнего файла не может быть отредактирован."]
      ItemCond c ->
        (\rc -> Located 0 0 (CICond rc)) <$> first (\e -> [DtoError path e]) (condToRaw c)
      ItemGroup g -> Located 0 0 . CIGroup <$> groupToLogic path g

-- | Условие DSL из DTO: здесь разбирается только вид операнда
-- (значение/дни/границы); совместимость «поле × оператор»
-- проверяется 'validatePlaylist'.
condToRaw :: CondDto -> Either Text RawCond
condToRaw (CondDto field op mval) = case op of
  "bare" -> Right (RBare field)
  "eq" -> cmp OpEq
  "ne" -> cmp OpNe
  "gt" -> cmp OpGt
  "ge" -> cmp OpGe
  "lt" -> cmp OpLt
  "le" -> cmp OpLe
  "before" -> cmp OpBefore
  "after" -> cmp OpAfter
  "contains" -> bin OpContains
  "notContains" -> bin OpNotContains
  "startsWith" -> bin OpStartsWith
  "endsWith" -> bin OpEndsWith
  "between" -> do
    v <- need
    if isDateField field
      then case arrVals v of
        [String a, String b] -> do
          da <- dateText a
          db <- dateText b
          pure (RDateBetween field da db)
        _ -> Left dateExpected
      else case arrVals v of
        [Number a, Number b] -> Right (RBetween field a b)
        _ -> Left "оператор «между» ожидает пару границ [от, до]"
  "inTheLast" -> RRelative field <$> needDays
  "notInTheLast" -> do
    days <- needDays
    if isNotPlayedField field
      then Right (RNotPlayed days)
      -- «Не за N дней» применимо к любому датовому полю ('isDateField'):
      -- у поля, отмеченного возможностью 'CapNotPlayed' в реестре,
      -- остаётся сахар 'RNotPlayed' — конкретное поле здесь не
      -- называется.
      else if isDateField field
        then Right (RNotRelative field days)
        else
          Left
            ( "оператор «не звучало N дней» применим только к датовым полям, получено поле «"
                <> field
                <> "»"
            )
  "isMissing" -> Right (RPresence field Absent)
  "isPresent" -> Right (RPresence field Present)
  "inPlaylist" -> playlist InPlaylist
  "notInPlaylist" -> playlist NotInPlaylist
  other -> Left ("неизвестный оператор «" <> other <> "»")
  where
    -- Членство в подборке: поле всегда «подборка», операнд DTO —
    -- объект {kind: "id"|"path", value: "..."}. Пустая ссылка
    -- проходит здесь структурно и отклоняется валидацией.
    playlist m
      | field /= playlistRefDslName =
          Left
            ( "оператор «"
                <> op
                <> "» применим только к полю «"
                <> playlistRefDslName
                <> "»"
            )
      | otherwise = do
          v <- need
          case v of
            Object fo -> do
              kind <- case KM.lookup "kind" fo of
                Just (String k) -> case k of
                  "id" -> Right RefId
                  "path" -> Right RefPath
                  _ -> Left (refKindErr k)
                _ -> Left refShapeDoc
              value <- case KM.lookup "value" fo of
                Just (String t) -> Right t
                _ -> Left refShapeDoc
              pure (RPlaylist m (PlaylistRef kind value))
            _ -> Left refShapeDoc

    refKindErr :: Text -> Text
    refKindErr k =
      "неизвестный вид ссылки на подборку «" <> k <> "», ожидается «id» или «path»"

    refShapeDoc :: Text
    refShapeDoc = "ожидается ссылка на подборку вида {kind: «id»|«path», value: «...»}"

    bin o = do
      v <- need
      rv <- toRawValue v
      pure (RBin field o rv)

    -- Оператор сравнения: на датовом поле операнд обязан быть датой
    -- @ГГГГ-ММ-ДД@, на остальных — прежний разбор значения.
    cmp o
      | isDateField field = RBin field o <$> dateOperand
      | otherwise = bin o

    dateOperand = do
      v <- need
      case v of
        String t -> RVDate <$> dateText t
        _ -> Left dateExpected

    dateText :: Text -> Either Text Day
    dateText t = maybe (Left dateExpected) Right (parseDay t)

    dateExpected :: Text
    dateExpected = "ожидается дата в формате ГГГГ-ММ-ДД"

    isDateField :: Text -> Bool
    isDateField fname = case fieldByName fname of
      Just (SomeField f) -> fieldValueType f == DateType
      Nothing -> False

    -- Поле — цель сахара «не звучало N дней»: признак берётся из
    -- реестра по возможности 'CapNotPlayed', а не по конкретному полю.
    isNotPlayedField :: Text -> Bool
    isNotPlayedField fname =
      maybe False (someFieldHasCapability CapNotPlayed) (fieldByName fname)

    need = case mval of
      Nothing -> Left ("условие с оператором «" <> op <> "» не содержит значение")
      Just v -> Right v

    needDays = do
      v <- need
      case v of
        Number n
          | isIntegral n && realToInteger n > 0 -> Right (realToInteger n)
          | isIntegral n -> Left "число дней должно быть положительным"
        _ -> Left "ожидается число дней"

    toRawValue = \case
      String t -> Right (RVText t)
      Bool b -> Right (RVBool b)
      -- Числа принимаются любые: дробный операнд целочисленного
      -- поля отклонит 'Nspeller.Validation' с точной ошибкой.
      Number n -> Right (RVNumber n)
      _ -> Left "неподдерживаемое значение условия"

------------------------------------------------------------------------------
-- Конвейер сохранения
------------------------------------------------------------------------------

-- | Результат конвейера: канонический @.mix@ и байты @.nsp@.
data Compiled = Compiled
  { cmpMix :: Text
  , cmpNsp :: LBS.ByteString
  }
  deriving (Eq, Show)

-- | Полный конвейер DTO → файлы.
--
-- Ошибки структуры получают путь до элемента; ошибки разбора и
-- валидации — строку и столбец в каноническом @.mix@, а путь до
-- элемента вычисляется по позиции в разобранном дереве.
--
-- Предупреждения возвращаются отдельным списком ('compilePlaylistDtoWarnings')
-- и на результат не влияют; здесь они отбрасываются.
compilePlaylistDto :: PlaylistDto -> Either [ApiError] Compiled
compilePlaylistDto = fmap fst . compilePlaylistDtoWarnings

-- | Как 'compilePlaylistDto', но возвращает также предупреждения
-- валидации — в формате 'ApiError' с кодом @"warning"@ и позицией в
-- каноническом @.mix@. Предупреждения появляются только при
-- успешной компиляции и никогда не останавливают её.
compilePlaylistDtoWarnings ::
  PlaylistDto ->
  Either [ApiError] (Compiled, [ApiError])
compilePlaylistDtoWarnings dto = do
  parsed0 <- first dtoErrorsToApi (dtoToParsed dto)
  let mix = renderParsedFile parsed0
  parsed <-
    first (compileErrorsToApi "parse" mix Nothing . pure) $
      parsePlaylist "<playlist>" mix
  (valid, warns) <-
    first (compileErrorsToApi "validation" mix (Just parsed)) $
      validatePlaylistWithWarnings "<playlist>" mix parsed
  pure
    ( Compiled mix (encodeNsp (toNsp valid))
    , compileErrorsToApi "warning" mix (Just parsed) warns
    )

-- | Превращение ошибок компиляции в структурированные ошибки API.
--
-- @defCode@ — код для ошибок с позицией ('"parse"' или
-- '"validation"'); ошибки без позиции (файловая система) получают
-- код @"io"@. Если передано разобранное дерево, к каждой ошибке
-- добавляется путь до элемента.
compileErrorsToApi :: Text -> Text -> Maybe ParsedFile -> [CompileError] -> [ApiError]
compileErrorsToApi defCode src mparsed errs = map convert errs
  where
    convert e = case cePos e of
      Nothing ->
        ApiError "io" (message e) Nothing Nothing Nothing
      Just pos ->
        ApiError
          defCode
          (message e)
          (case mparsed of
             Nothing -> Nothing
             Just p ->
               let path = pathAtOffset p (offsetFromPos src pos)
                in if T.null path then Nothing else Just path)
          (Just (fst pos))
          (Just (snd pos))
    message e = T.intercalate "\n" (NE.toList (ceMessages e))

------------------------------------------------------------------------------
-- ValidPlaylist → DTO
------------------------------------------------------------------------------

-- | Перевод валидированного AST в DTO (обратная операция конвейера).
validToDto :: ValidPlaylist -> PlaylistDto
validToDto vp =
  PlaylistDto
    { pdName = vpName vp
    , pdDescription = vpDescription vp
    , pdPublic = vpPublic vp
    , pdRoot = groupDto (vpRoot vp)
    , pdSort = sortDto <$> vpSort vp
    , pdLimit = vpLimit vp
    }
  where
    groupDto (ValidGroup kind items) =
      GroupDto (case kind of All -> "all"; Any -> "any") (map itemDto items)
    itemDto = \case
      VIC c -> condItemDto c
      VIG g -> ItemGroup (groupDto g)
    sortDto = \case
      SortRandomMode -> SortRandomDto
      SortBy items ->
        SortFieldsDto [SortItemDto f (dir d) | SortItem f d <- items]
    dir SortAsc = "asc"
    dir SortDesc = "desc"

-- | Условие валидированного AST → DTO.
condItemDto :: ValidCond -> ItemDto
condItemDto = ItemCond . condDto
  where
    condDto = \case
      VText f op t -> CondDto (fieldDslName f) (textOpId op) (Just (String t))
      VNumber f op n -> CondDto (fieldDslName f) (numOpId op) (Just (numValue n))
      VBetween f lo hi ->
        CondDto
          (fieldDslName f)
          "between"
          (Just (toJSON [numValue lo, numValue hi] :: Value))
      VBool f True -> CondDto (fieldDslName f) "bare" Nothing
      VBool f False -> CondDto (fieldDslName f) "eq" (Just (Bool False))
      VRelative f InTheLast days ->
        CondDto (fieldDslName f) "inTheLast" (Just (numValue (fromIntegral days)))
      VRelative f NotInTheLast days ->
        CondDto (fieldDslName f) "notInTheLast" (Just (numValue (fromIntegral days)))
      VDate f op d ->
        CondDto (fieldDslName f) (dateOpId op) (Just (String (formatDay d)))
      VDateRange f lo hi ->
        CondDto
          (fieldDslName f)
          "between"
          (Just (toJSON [String (formatDay lo), String (formatDay hi)] :: Value))
      VPresence (SomeField f) Absent -> CondDto (fieldDslName f) "isMissing" Nothing
      VPresence (SomeField f) Present -> CondDto (fieldDslName f) "isPresent" Nothing
      VPlaylist m ref ->
        CondDto playlistRefDslName (membershipOpId m) (Just (playlistRefDtoValue ref))

    numValue :: Scientific -> Value
    numValue n = Number n

textOpId :: TextOp -> Text
textOpId = \case
  TEq -> "eq"
  TNe -> "ne"
  TContains -> "contains"
  TNotContains -> "notContains"
  TStartsWith -> "startsWith"
  TEndsWith -> "endsWith"

numOpId :: NumOp -> Text
numOpId = \case
  NEq -> "eq"
  NNe -> "ne"
  NGt -> "gt"
  NGe -> "ge"
  NLt -> "lt"
  NLe -> "le"

dateOpId :: DateOp -> Text
dateOpId = \case
  DEq -> "eq"
  DNe -> "ne"
  DGt -> "gt"
  DGe -> "ge"
  DLt -> "lt"
  DLe -> "le"
  DBefore -> "before"
  DAfter -> "after"

-- | Идентификатор DSL оператора членства в подборке.
membershipOpId :: PlaylistMembership -> Text
membershipOpId = \case
  InPlaylist -> "inPlaylist"
  NotInPlaylist -> "notInPlaylist"

-- | Ссылка на подборку → JSON-операнд DTO: @{"kind": "id"|"path",
-- "value": "..."}@.
playlistRefDtoValue :: PlaylistRef -> Value
playlistRefDtoValue (PlaylistRef kind value) =
  object ["kind" .= playlistRefKindId kind, "value" .= value]

------------------------------------------------------------------------------
-- .nsp JSON → DTO
------------------------------------------------------------------------------

-- | Разбор @.nsp@ в DTO.
--
-- Неизвестные поля, операторы и значения превращаются в 'ItemRaw'
-- (или 'SortRawDto') — они сохраняются при чтении, но блокируют
-- редактирование, поскольку не выражимы в DSL.
nspToDto :: Value -> Either Text PlaylistDto
nspToDto (Object o) = do
  name <- case KM.lookup "name" o of
    Just (String n) -> Right n
    _ -> Left "в .nsp отсутствует обязательное поле name"
  let desc = case KM.lookup "comment" o of
        Just (String d) -> Just d
        _ -> Nothing
      pub = case KM.lookup "public" o of
        Just (Bool b) -> b
        _ -> False
  (kind, rootVal) <- case (KM.lookup "all" o, KM.lookup "any" o) of
    (Just _, Just _) -> Left "в .nsp одновременно заданы all и any"
    (Just v, Nothing) -> Right ("all", v)
    (Nothing, Just v) -> Right ("any", v)
    (Nothing, Nothing) -> Left "в .nsp отсутствует корневая группа all/any"
  items <- case rootVal of
    Array _ -> Right (map condItemFromNsp (arrVals rootVal))
    _ -> Left "корневая группа .nsp должна быть массивом"
  sortDto <- case KM.lookup "sort" o of
    Nothing -> Right Nothing
    Just (String t) -> Right (nspSortDto t)
    Just _ -> Right (Just (SortRawDto "(значение sort не является строкой)"))
  limit <- case KM.lookup "limit" o of
    Nothing -> Right Nothing
    Just (Number n)
      | isIntegral n -> Right (Just (realToInteger n))
    Just _ -> Left "лимит .nsp должен быть целым числом"
  pure $
    PlaylistDto
      { pdName = name
      , pdDescription = desc
      , pdPublic = pub
      , pdRoot = GroupDto kind items
      , pdSort = sortDto
      , pdLimit = limit
      }
nspToDto _ = Left "файл .nsp должен быть JSON-объектом"

-- | Условие из @.nsp@ → элемент DTO; неизвестное — raw-узел,
-- сохраняющий исходный JSON без изменений.
condItemFromNsp :: Value -> ItemDto
condItemFromNsp v = case v of
  Object o
    | [(k, val)] <- KM.toList o -> case Key.toString k of
        "all" -> grp "all" val
        "any" -> grp "any" val
        -- inPlaylist/notInPlaylist разбираются до binOp: их операнд —
        -- ссылка {"id"|"path": строка}, а не поле-значение.
        "inPlaylist" -> playlistCond "inPlaylist" val
        "notInPlaylist" -> playlistCond "notInPlaylist" val
        opName -> binOp opName val
  _ -> ItemRaw v
  where
    grp kind val = case arrVals val of
      [] | not (isArray val) -> ItemRaw v
      vals -> ItemGroup (GroupDto kind (map condItemFromNsp vals))

    -- Ссылка на подборку → условие DTO; невыражимая ссылка (не
    -- объект, не строка, неизвестный вид) остаётся raw-узлом.
    playlistCond opName val = case val of
      Object fo -> case KM.toList fo of
        [(fk, String t)] -> case Key.toString fk of
          "id" ->
            ItemCond (CondDto playlistRefDslName opName (Just (linkValue "id" t)))
          "path" ->
            ItemCond (CondDto playlistRefDslName opName (Just (linkValue "path" t)))
          _ -> ItemRaw v
        _ -> ItemRaw v
      _ -> ItemRaw v

    linkValue kind t = object ["kind" .= (kind :: Text), "value" .= t]

    binOp opName val = case val of
      Object fo
        | [(fk, fv)] <- KM.toList fo ->
            case (opDsl opName, fieldDsl (Key.toString fk), nspCondValue opName (Key.toString fk) fv) of
              (Just op, Just fld, Just mval) -> ItemCond (CondDto fld op mval)
              _ -> ItemRaw v
      _ -> ItemRaw v

    fieldDsl n = case fieldByName (T.pack n) of
      Just (SomeField f) -> Just (fieldDslName f)
      Nothing -> Nothing

    isDateNsp n = case fieldByName (T.pack n) of
      Just (SomeField f) -> fieldValueType f == DateType
      Nothing -> False

    -- Оператор NSP → идентификатор DSL; Nothing — неизвестный.
    opDsl = \case
      "is" -> Just "eq"
      "isNot" -> Just "ne"
      "gt" -> Just "gt"
      "lt" -> Just "lt"
      "contains" -> Just "contains"
      "notContains" -> Just "notContains"
      "startsWith" -> Just "startsWith"
      "endsWith" -> Just "endsWith"
      "inTheRange" -> Just "between"
      "inTheLast" -> Just "inTheLast"
      "notInTheLast" -> Just "notInTheLast"
      "before" -> Just "before"
      "after" -> Just "after"
      "isMissing" -> Just "isMissing"
      "isPresent" -> Just "isPresent"
      _ -> Nothing

    -- Значение условия NSP → операнд DTO: Just (Just v) — со значением,
    -- Just Nothing — без значения (проверка наличия),
    -- Nothing — не выражимо в DSL.
    nspCondValue opName nspName fv = case opName of
      "is" -> Just (Just fv)
      "isNot" -> Just (Just fv)
      -- gt/lt: число для числовых полей, строка-дата для датовых.
      "gt" -> cmpValue nspName fv
      "lt" -> cmpValue nspName fv
      "before" -> dateOnly fv
      "after" -> dateOnly fv
      "contains" -> strOnly fv
      "notContains" -> strOnly fv
      "startsWith" -> strOnly fv
      "endsWith" -> strOnly fv
      "inTheRange" -> case arrVals fv of
        [Number _, Number _] -> Just (Just fv)
        [String a, String b]
          | isDateNsp nspName && isJust (parseDay a) && isJust (parseDay b) ->
              Just (Just fv)
        _ -> Nothing
      "inTheLast" -> daysOnly fv
      "notInTheLast" -> daysOnly fv
      "isMissing" -> boolTrue fv
      "isPresent" -> boolTrue fv
      _ -> Nothing

    -- Числовые операнды принимаются любыми (дробные в т. ч.):
    -- дробность запрещает только валидация целочисленных полей.
    numOnly fv = case fv of
      Number _ -> Just (Just fv)
      _ -> Nothing
    strOnly fv = case fv of
      String _ -> Just (Just fv)
      _ -> Nothing
    daysOnly fv = case fv of
      Number n | isIntegral n && realToInteger n > 0 -> Just (Just fv)
      _ -> Nothing
    boolTrue fv = case fv of
      Bool True -> Just Nothing
      _ -> Nothing
    -- Абсолютная дата: строка @ГГГГ-ММ-ДД@ (см. 'parseDay').
    dateOnly fv = case fv of
      String t | isJust (parseDay t) -> Just (Just fv)
      _ -> Nothing
    cmpValue nspName fv
      | isDateNsp nspName = dateOnly fv
      | otherwise = numOnly fv

-- | Строка сортировки @.nsp@ → DTO; неизвестные поля — сырой текст.
nspSortDto :: Text -> Maybe SortDto
nspSortDto t
  | t == "random" = Just SortRandomDto
  | T.null t = Just (SortRawDto t)
  | otherwise =
      let items = map parseItem (T.splitOn "," t)
       in if any isNothing items
            then Just (SortRawDto t)
            else Just (SortFieldsDto [i | Just i <- items])
  where
    parseItem chunk =
      let (dir, name) = case T.stripPrefix "-" (T.strip chunk) of
            Just rest -> ("desc", rest)
            Nothing -> ("asc", T.strip chunk)
       in case sortFieldByName name of
            Just f -> Just (SortItemDto f dir)
            Nothing -> Nothing

------------------------------------------------------------------------------
-- .mix текст → DTO
------------------------------------------------------------------------------

-- | Разбор содержимого @.mix@ в DTO (используется Store'ом для
-- managed-подборок).
dtoFromMix :: Text -> Either [ApiError] PlaylistDto
dtoFromMix src = do
  parsed <-
    first (compileErrorsToApi "parse" src Nothing . pure) $
      parsePlaylist "<playlist>" src
  valid <-
    first (compileErrorsToApi "validation" src (Just parsed)) $
      validatePlaylist "<playlist>" src parsed
  pure (validToDto valid)

------------------------------------------------------------------------------
-- Позиции → путь
------------------------------------------------------------------------------

-- | Обратная операция 'posFromOffset': позиция (строка, столбец) →
-- смещение в исходнике (обрезается по границам файла).
offsetFromPos :: Text -> (Int, Int) -> Int
offsetFromPos src (ln, col) =
  let ls = T.splitOn "\n" src
      n = max 0 (ln - 1)
      fullLines = take n ls
      lineLen = maybe 0 T.length (atMay ls n)
      start = sum [T.length l + 1 | l <- fullLines]
      off = start + max 0 (min (col - 1) lineLen)
   in max 0 (min off (T.length src))
  where
    atMay xs i = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

------------------------------------------------------------------------------
-- Структура → путь (для ошибок валидации)
------------------------------------------------------------------------------

-- | Путь до элемента дерева, соответствующего смещению @off@ в
-- исходнике. Предпочитается узел, начинающийся точно на ошибке
-- (так ошибки валидации указывают на элемент, а не на родителя);
-- при нескольких совпадениях берётся самое вложенное.
pathAtOffset :: ParsedFile -> Int -> Text
pathAtOffset (ParsedFile stmts) off =
  case filter startsHere nodes of
    [] -> case filter containsIt nodes of
      [] -> ""
      cs -> nodePath (last (sortBy (comparing nodeStart) cs))
    ms -> nodePath (last ms)
  where
    nodes = concatMap statementNodes (zip [0 :: Int ..] stmts)
    startsHere n = nodeStart n == off
    containsIt n = nodeStart n <= off && off <= nodeEnd n

-- | Узел дерева путей: путь до элемента DTO и позиция его текста.
data PathNode = PathNode
  { nodePath :: Text
  , nodeStart :: Int
  , nodeEnd :: Int
  }

statementNodes :: (Int, Located Statement) -> [PathNode]
statementNodes (_i, Located s e stmt) = case stmt of
  SName _ -> [PathNode "/name" s e]
  SDescription _ -> [PathNode "/description" s e]
  SPublic -> [PathNode "/public" s e]
  SSort _ -> [PathNode "/sort" s e]
  SLimit _ -> [PathNode "/limit" s e]
  SWhere g -> PathNode "/root" s e : groupNodes "/root" g

groupNodes :: Text -> LogicGroup -> [PathNode]
groupNodes path (LogicGroup _ items) =
  concat
    [ itemNodes (path <> "/items/" <> tshow j) it
    | (j, it) <- zip [0 :: Int ..] items
    ]

itemNodes :: Text -> Located CondItem -> [PathNode]
itemNodes path (Located s e item) = case item of
  CICond _ -> [PathNode path s e]
  CIGroup g -> PathNode path s e : groupNodes path g

------------------------------------------------------------------------------
-- Общие помощники
------------------------------------------------------------------------------

isArray :: Value -> Bool
isArray (Array _) = True
isArray _ = False

arrVals :: Value -> [Value]
arrVals (Array v) = foldr (:) [] v
arrVals _ = []

isIntegral :: Real a => a -> Bool
isIntegral x = denominator (toRational x) == 1

realToInteger :: Real a => a -> Integer
realToInteger = round . toRational

tshow :: Show a => a -> Text
tshow = T.pack . show
