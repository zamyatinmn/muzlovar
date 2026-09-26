{-# LANGUAGE OverloadedStrings #-}

module BilingualTests (bilingualTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (Value (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nspeller.Ast (ValidPlaylist, renderCompileError)
import Nspeller.Dialect
import Nspeller.Fields
import Nspeller.Muzlovar.Types
import Nspeller.Navidrome (encodeNsp, toNsp)
import Nspeller.Parser
import Nspeller.Render
import Nspeller.Validation (validatePlaylist)
import System.FilePath ((</>), (<.>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

parseValid :: T.Text -> IO ValidPlaylist
parseValid src = case parsePlaylist "test.mix" src of
  Left e -> assertFailure (T.unpack (renderCompileError e))
  Right p -> case validatePlaylist "test.mix" src p of
    Left es -> assertFailure (show es)
    Right v -> pure v

parseSource :: T.Text -> IO ()
parseSource src = case parsePlaylist "test.mix" src of
  Left e -> assertFailure (T.unpack (renderCompileError e))
  Right _ -> pure ()

goldenEquivalence :: String -> TestTree
goldenEquivalence name = testCase name $ do
  bs <- BS.readFile ("test" </> "golden" </> name <.> "mix")
  ru <- case TE.decodeUtf8' bs of
    Left e -> assertFailure (show e)
    Right t -> pure t
  parsed <- case parsePlaylist "golden.mix" ru of
    Left e -> assertFailure (T.unpack (renderCompileError e))
    Right p -> pure p
  let en = renderParsedFileIn En parsed
      canonicalRu = renderParsedFileIn Ru parsed
  parseSource en
  ruAst <- parseValid canonicalRu
  enAst <- parseValid en
  enAst @?= ruAst
  encodeNsp (toNsp enAst) @?= encodeNsp (toNsp ruAst)
  detectDslDialect "ru.mix" canonicalRu @?= Just Ru
  detectDslDialect "en.mix" en @?= Just En

fieldMappings :: TestTree
fieldMappings = testCase "all registered field names resolve to the same ID" $
  mapM_ check fieldSpecs
  where
    check (SomeFieldSpec spec) = do
      let ru = spDslName spec
          en = spNspName spec
      assertBool ("missing EN token: " <> T.unpack ru) (not (T.null en))
      if not (specHasCapability CapStatic spec)
        then do
          ru @?= playlistRefDslName
          en @?= "inPlaylist"
        else case (fieldByName ru, fieldByName en) of
          (Just (SomeField r), Just (SomeField e)) -> do
            fieldName r @?= fieldName e
            fieldDslName r @?= fieldDslName e
          _ -> assertFailure ("unresolved mapping: " <> T.unpack ru <> " -> " <> T.unpack en)

keywords :: TestTree
keywords = testCase "every keyword has both spellings" $
  mapM_ check [minBound .. maxBound]
  where
    check k = do
      assertBool (show k) (not (T.null (keyword Ru k)))
      assertBool (show k) (not (T.null (keyword En k)))

operators :: TestTree
operators = testCase "multilingual operators and mixed syntax" $ do
  let en = T.unlines
        [ "playlist \"Same title\"", "description \"User text\"", "public"
        , "where all {"
        , "  genre contains \"rock\""
        , "  title does not contain \"live\""
        , "  artist starts with \"A\""
        , "  album ends with \"B\""
        , "  year between 1990 and 2020"
        , "  playCount >= 1"
        , "  added within 30 days"
        , "  added not within 90 days"
        , "  lastPlayed before 2025-01-01"
        , "  added after 2020-01-01"
        , "  added between 2020-01-01 and 2025-01-01"
        , "  loved = true"
        , "  loved = false"
        , "  album is missing"
        , "  album is present"
        , "  any {", "    not played within 60 days", "    in playlist id \"abc\"", "    not in playlist file \"other.nsp\"", "  }"
        , "}", "sort {", "  title ascending", "  year descending", "}", "limit 50"
        ]
  parseSource en
  parsed <- case parsePlaylist "en.mix" en of
    Left e -> assertFailure (T.unpack (renderCompileError e))
    Right p -> pure p
  let ru = renderParsedFileIn Ru parsed
  parseSource ru
  -- The validation result is shared even when the source includes
  -- combinations that trigger semantic diagnostics.
  case (parsePlaylist "ru.mix" ru, parsePlaylist "en.mix" (renderParsedFileIn En parsed)) of
    (Right r, Right e) -> do
      renderParsedFileIn Ru e @?= renderParsedFileIn Ru r
      renderParsedFileIn En r @?= renderParsedFileIn En e
    other -> assertFailure (show other)
  let mixed = "playlist \"X\"\nwhere any {\n  жанр contains \"rock\"\n}\n"
  parseSource mixed
  detectDslDialect "mixed.mix" mixed @?= Nothing
  assertBool "invalid operator accepted" $ case parsePlaylist "bad.mix" "playlist \"X\"\nwhere all { genre not contain \"x\" }" of
    Left _ -> True
    Right _ -> False

dtoEquivalence :: TestTree
dtoEquivalence = testCase "DTO renders different .mix and identical .nsp" $ do
  let dto = PlaylistDto "Same title" (Just "User text") True
        (GroupDto "any" [ItemCond (CondDto "жанр" "contains" (Just (String "rock"))),
                         ItemCond (CondDto "добавлено" "inTheLast" (Just (Number 30)))])
        (Just (SortFieldsDto [SortItemDto "год" "desc"])) (Just 20)
  ru <- case compilePlaylistDtoIn Ru dto of
    Left es -> assertFailure (show es)
    Right c -> pure c
  en <- case compilePlaylistDtoIn En dto of
    Left es -> assertFailure (show es)
    Right c -> pure c
  assertBool "dialects rendered identically" (cmpMix ru /= cmpMix en)
  cmpNsp ru @?= cmpNsp en
  dtoFromMix (cmpMix ru) @?= Right dto
  dtoFromMix (cmpMix en) @?= Right dto
  compilePlaylistDto dto @?= Right ru

relativeSemantics :: TestTree
relativeSemantics = testCase "relative dates and playback shorthand keep their existing semantics" $ do
  let wrapRu rule = "подборка \"X\"\nгде все { " <> rule <> " }\n"
      wrapEn rule = "playlist \"X\"\nwhere all { " <> rule <> " }\n"
      ruPlayed = wrapRu "не звучало 30 дней"
      ruExplicit = wrapRu "последнее_прослушивание не за 30 дней"
      enPlayed = wrapEn "not played within 30 days"
      enExplicit = wrapEn "lastplayed not within 30 days"
  played <- parseValid ruPlayed
  mapM_ (\src -> parseValid src >>= (@?= played)) [ruExplicit, enPlayed, enExplicit]
  let playedNsp = TE.decodeUtf8 (LBS.toStrict (encodeNsp (toNsp played)))
  assertBool "playback field missing" ("\"notInTheLast\"" `T.isInfixOf` playedNsp && "\"lastplayed\"" `T.isInfixOf` playedNsp)
  parsed <- case parsePlaylist "explicit.mix" ruExplicit of
    Left e -> assertFailure (T.unpack (renderCompileError e))
    Right p -> pure p
  renderParsedFileIn Ru parsed @?= "подборка \"X\"\nгде все {\n  не звучало 30 дней\n}\n"
  renderParsedFileIn En parsed @?= "playlist \"X\"\nwhere all {\n  not played within 30 days\n}\n"
  addedRu <- parseValid (wrapRu "добавлено не за 30 дней")
  addedEn <- parseValid (wrapEn "dateadded not within 30 days")
  addedRu @?= addedEn
  assertBool "generic date unexpectedly became playback field" (addedRu /= played)
  let addedNsp = TE.decodeUtf8 (LBS.toStrict (encodeNsp (toNsp addedRu)))
  assertBool "generic date field missing" ("\"notInTheLast\"" `T.isInfixOf` addedNsp && "\"dateadded\"" `T.isInfixOf` addedNsp)
  withinRu <- parseValid (wrapRu "добавлено за 30 дней")
  withinEn <- parseValid (wrapEn "dateadded within 30 days")
  withinRu @?= withinEn
  let withinNsp = TE.decodeUtf8 (LBS.toStrict (encodeNsp (toNsp withinRu)))
  assertBool "positive relative operator missing" ("\"inTheLast\"" `T.isInfixOf` withinNsp && "\"dateadded\"" `T.isInfixOf` withinNsp)
  let bad = wrapEn "dateadded not played within 30 days"
  case parsePlaylist "bad.mix" bad of
    Left _ -> pure ()
    Right p -> case validatePlaylist "bad.mix" bad p of
      Left _ -> pure ()
      Right _ -> assertFailure "playback shorthand accepted as an operator on dateadded"

bilingualTests :: TestTree
bilingualTests = testGroup "Bilingual DSL"
  [ keywords, fieldMappings, operators, dtoEquivalence, relativeSemantics
  , testGroup "existing RU fixtures and EN round trips"
      (map goldenEquivalence ["forgotten-favorites", "eighties-rock", "missing-metadata", "recent-discoveries", "playlist-links"])
  ]
