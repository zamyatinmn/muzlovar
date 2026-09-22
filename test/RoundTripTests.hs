{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip «парсер ⇄ рендер»: для golden-файлов и DTO проверяется,
-- что @parse (render ast)@ совпадает с @ast@ с точностью до позиций
-- ('Located'), а рендер идемпотентен.
module RoundTripTests (roundTripTests) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Generators (genPlaylist)
import Nspeller.Ast
import Nspeller.Muzlovar.Types (PlaylistDto (pdName), dtoToParsed, validToDto)
import Nspeller.Parser (parsePlaylist)
import Nspeller.Render (renderParsedFile)
import System.FilePath ((</>), (<.>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, counterexample, forAll, testProperty, (===))

------------------------------------------------------------------------------
-- Обнуление позиций
------------------------------------------------------------------------------

-- | Позиции верхнеуровневых секций -> нули.
stripParsed :: ParsedFile -> ParsedFile
stripParsed (ParsedFile stmts) = ParsedFile (map stripLocatedStmt stmts)

stripLocatedStmt :: Located Statement -> Located Statement
stripLocatedStmt (Located _ _ s) = Located 0 0 (stripStatement s)

stripStatement :: Statement -> Statement
stripStatement = \case
  SName t -> SName t
  SDescription t -> SDescription t
  SPublic -> SPublic
  SWhere g -> SWhere (stripGroup g)
  SSort s -> SSort (stripSort s)
  SLimit n -> SLimit n

stripGroup :: LogicGroup -> LogicGroup
stripGroup (LogicGroup kind items) = LogicGroup kind (map stripLocatedItem items)

stripLocatedItem :: Located CondItem -> Located CondItem
stripLocatedItem (Located _ _ i) = Located 0 0 (stripCondItem i)

stripCondItem :: CondItem -> CondItem
stripCondItem = \case
  CICond c -> CICond c
  CIGroup g -> CIGroup (stripGroup g)

stripSort :: RawSort -> RawSort
stripSort = \case
  SortRandom -> SortRandom
  SortSpec items -> SortSpec [stripLocatedSortItem i | i <- items]

stripLocatedSortItem :: Located RawSortItem -> Located RawSortItem
stripLocatedSortItem (Located _ _ (RawSortItem f d)) = Located 0 0 (RawSortItem f d)

------------------------------------------------------------------------------
-- Golden-файлы
------------------------------------------------------------------------------

goldenDir :: FilePath
goldenDir = "test" </> "golden"

-- | Прочитать golden @.mix@ и разобрать.
parseGolden :: String -> IO ParsedFile
parseGolden name = do
  bs <- BS.readFile (goldenDir </> name <.> "mix")
  case TE.decodeUtf8' bs of
    Left _ -> assertFailure ("golden-файл не в UTF-8: " <> name)
    Right src -> parseOrFail (name <.> "mix") src

parseOrFail :: FilePath -> Text -> IO ParsedFile
parseOrFail fp src = case parsePlaylist fp src of
  Left e -> assertFailure ("ошибка разбора:\n" <> T.unpack (renderCompileError e))
  Right p -> pure p

-- | @parse (render (parse src)) == parse src@ (без позиций) и рендер
-- идемпотентен.
goldenRoundTrip :: String -> TestTree
goldenRoundTrip name = testCase name $ do
  p1 <- parseGolden name
  let r1 = renderParsedFile p1
  p2 <- parseOrFail (name <.> "mix") r1
  stripParsed p2 @?= stripParsed p1
  renderParsedFile p2 @?= r1

-- | Регрессия: «чистое» булево условие непосредственно перед
-- @не звучало N дней@. Лексема предыдущего условия переносит позицию
-- на новую строку, и оператор «не содержит» пытается съесть её
-- начало — без защиты разбор падал.
bareThenNotPlayed :: TestTree
bareThenNotPlayed = testCase "обложка + не звучало на следующей строке" $ do
  let src =
        "подборка \"тест\"\n\
        \где все {\n\
        \  обложка\n\
        \  не звучало 90 дней\n\
        \}\n"
  p1 <- parseOrFail "test.mix" src
  let r1 = renderParsedFile p1
  p2 <- parseOrFail "test.mix" r1
  stripParsed p2 @?= stripParsed p1
  renderParsedFile p2 @?= r1

------------------------------------------------------------------------------
-- Свойство для сгенерированных подборок
------------------------------------------------------------------------------

-- | DTO -> рендер -> разбор сходятся с точностью до позиций.
-- Повторная валидация не выполняется: генератор и так выдаёт
-- валидированный AST, а здесь проверяется только обратимость
-- рендера.
prop_dtoRenderParse :: Property
prop_dtoRenderParse = forAll genPlaylist $ \vp ->
  let dto0 = validToDto vp
      dto =
        if T.null (T.strip (pdName dto0))
          then dto0 {pdName = "Подборка"}
          else dto0
   in case dtoToParsed dto of
        Left es -> counterexample ("dtoToParsed: " <> show es) False
        Right p1 -> case parsePlaylist "<генератор>" (renderParsedFile p1) of
          Left e -> counterexample ("parsePlaylist: " <> T.unpack (renderCompileError e)) False
          Right p2 -> stripParsed p2 === stripParsed p1

------------------------------------------------------------------------------
-- Итоговый набор
------------------------------------------------------------------------------

roundTripTests :: TestTree
roundTripTests =
  testGroup
    "RoundTrip"
    [ testGroup
        "Golden-файлы: parse -> render -> parse"
        [ goldenRoundTrip "forgotten-favorites"
        , goldenRoundTrip "eighties-rock"
        , goldenRoundTrip "missing-metadata"
        ]
    , bareThenNotPlayed
    , testProperty "DTO -> render -> parse сходится" prop_dtoRenderParse
    ]
