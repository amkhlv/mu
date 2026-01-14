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
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

commandParser :: Parser MyCommand
commandParser =
  $(mkCommandParserExp)

opts :: ParserInfo MyCommand
opts = info (commandParser <**> helper) idm

runShellCommand :: MyCommand -> IO (Either String String)
runShellCommand (MyCommand cmd) = do
  result <- (Right <$> readProcess "sh" ["-c", cmd] "") `catch` handleShellError
  return result
  where
    handleShellError :: SomeException -> IO (Either String String)
    handleShellError e = return $ Left $ "Error executing command: " ++ show e

main :: IO ()
main = do
  options <- execParser opts
  putStrLn $ show options
  result <- runShellCommand options
  case result of
    Right output -> putStrLn output
    Left error -> hPutStrLn stderr error
