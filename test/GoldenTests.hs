{-# LANGUAGE OverloadedStrings #-}

-- | Golden-тесты: полные сценарии @.mix -> .nsp@ сравниваются с
-- эталонными файлами байт-в-байт. Это же фиксирует детерминированность
-- сериализации (порядок и форматирование ключей) между запусками.
module GoldenTests (goldenTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nspeller.Ast (renderCompileError)
import Nspeller.Compiler (compileText)
import Nspeller.Navidrome (encodeNsp)
import System.FilePath ((</>), (<.>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)

goldenDir :: FilePath
goldenDir = "test" </> "golden"

-- | Один сценарий: @test\/golden\/NAME.mix@ -> сравнение с
-- @test\/golden\/NAME.nsp@.
goldenMix :: String -> TestTree
goldenMix name = goldenVsString name (goldenDir </> name <.> "nsp") action
  where
    action = do
      bytes <- BS.readFile (goldenDir </> name <.> "mix")
      pure $ case TE.decodeUtf8' bytes of
        Left _ -> "не удалось декодировать .mix как UTF-8\n"
        Right src -> case compileText (name <.> "mix") src of
          Left errs ->
            LBS.fromStrict . TE.encodeUtf8 . T.unlines $
              map renderCompileError errs
          Right nsp -> encodeNsp nsp

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden"
    [ goldenMix "forgotten-favorites"
    , goldenMix "eighties-rock"
    , goldenMix "missing-metadata"
    , goldenMix "recent-discoveries"
    , goldenMix "playlist-links"
    ]
