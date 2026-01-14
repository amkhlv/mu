{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Cli
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Yaml as Yaml
import Language.Haskell.TH
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { inputFile :: FilePath,
    compactOutput :: Bool
  }

commandParser :: Parser MyCommand
commandParser =
  $( mkCommandParserExp
 --      [ ("ls", "List files in long format"),
 --        ("cert", "Certificate operations")
 --      ]
   )

opts :: ParserInfo MyCommand
opts = info (commandParser <**> helper) idm

main :: IO ()
main = do
  options <- execParser opts
  putStrLn $ show options
