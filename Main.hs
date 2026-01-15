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
import System.Posix.Process (executeFile)


commandParser :: Parser MyCommand
commandParser =
  $(mkCommandParserExp)

opts :: ParserInfo MyCommand
opts = info (commandParser <**> helper) idm

runShellCommand :: MyCommand -> IO ()
runShellCommand (MyCommand cmd) =
  executeFile "sh" True ["-c", cmd] Nothing

main :: IO ()
main = do
  options <- execParser opts
  putStrLn $ show options
  runShellCommand options
