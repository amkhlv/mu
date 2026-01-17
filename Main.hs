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
import Data.Char (isDigit)
import Data.List (intercalate)

type DryRun = Bool

data CommandAndArgs = CommandAndArgs MyCommand [String] DryRun

commandParser :: Parser MyCommand
commandParser =
  $(mkCommandParserExp)

comArgParser :: Parser CommandAndArgs
comArgParser = CommandAndArgs <$> commandParser <*> many (argument str
      (  metavar "ARGUMENTS..."
      <> help "One or more arguments"
      )) <*> switch
      (  long "dry-run"
      <> short 'n'
      <> help "Dry run mode"
      )

opts :: ParserInfo CommandAndArgs
opts = info (comArgParser <**> helper) idm


expand :: String -> [String] -> String
expand [] _ = []
expand ('%':cs) xs =
  let (ds, rest) = span isDigit cs
  in case ds of
       "" -> case rest of
               '@':rest1 -> intercalate " " xs ++ expand rest1 xs
               _ -> '%' : expand cs xs
       _  -> maybe (expand rest xs)
                   (++ expand rest xs)
                   (index xs ds)
expand (c:cs) xs = c : expand cs xs

index :: [a] -> String -> Maybe a
index xs ds =
  let n = read ds
  in if n > 0 && n <= length xs then Just (xs !! (n - 1)) else Nothing


runShellCommand :: CommandAndArgs -> IO ()
runShellCommand (CommandAndArgs (MyCommand cmd) args dryRun) =
  if dryRun then putStrLn $ expand cmd args else executeFile "sh" True ["-c", expand cmd args] Nothing

main :: IO ()
main = do
  options <- execParser opts
  runShellCommand options
