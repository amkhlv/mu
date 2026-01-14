{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            result <- Yaml.decodeFileEither filename :: IO (Either Yaml.ParseException Aeson.Value)
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Error parsing YAML file: " ++ show err
                    exitFailure
                Right value -> do
                    BL.putStrLn $ Pretty.encodePretty value
        _ -> do
            hPutStrLn stderr "Usage: yaml-reader <filename.yaml>"
            exitFailure
