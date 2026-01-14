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


commandParser :: Parser MyCommand
commandParser =
  $(mkCommandParserExp)

opts :: ParserInfo MyCommand
opts = info (commandParser <**> helper) idm

main :: IO ()
main = do
  options <- execParser opts
  putStrLn $ show options
