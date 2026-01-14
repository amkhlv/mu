{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Options = Options
    { inputFile :: FilePath
    , compactOutput :: Bool
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> argument str
        ( metavar "FILE"
       <> help "YAML file to read and convert to JSON" )
    <*> switch
        ( long "compact"
       <> short 'c'
       <> help "Output compact JSON instead of pretty-printed" )

versionOption :: Parser (a -> a)
versionOption = infoOption "yaml-reader version 0.1.0.0"
    ( long "version"
   <> short 'v'
   <> help "Show version information" )

opts :: ParserInfo Options
opts = info (optionsParser <**> helper <**> versionOption)
    ( fullDesc
   <> progDesc "Read a YAML file and print it as JSON"
   <> header "yaml-reader - a simple YAML to JSON converter" )

main :: IO ()
main = do
    options <- execParser opts
    result <- Yaml.decodeFileEither (inputFile options) :: IO (Either Yaml.ParseException Aeson.Value)
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error parsing YAML file: " ++ show err
            exitFailure
        Right value -> do
            if compactOutput options
                then BL.putStrLn $ Aeson.encode value
                else BL.putStrLn $ Pretty.encodePretty value
