-- | Точка входа исполняемого файла @nspeller@.
module Main (main) where

import Nspeller.Cli (runCli)

main :: IO ()
main = runCli
