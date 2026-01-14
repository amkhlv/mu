{-# LANGUAGE TemplateHaskell #-}

module Cli where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Yaml as Yaml
import qualified Control.Monad.IO.Class as CM
import Language.Haskell.TH
import qualified Data.HashMap.Strict as HM

type Actions = [(String, String)]

processObject :: Aeson.Object -> [(String, String)]
processObject obj = HM.toList obj


getYAML :: IO Actions
getYAML = do
  yaml <- Yaml.decodeFileEither "example.yaml"
  case yaml of
    Right value -> return $ processObject value
    Left err -> fail $ "YAML Error: " ++ Yaml.prettyPrintParseException err

data MyCommand = MyCommand String deriving (Show)

mkCommandParserExp :: Q Exp
mkCommandParserExp  = do
  yaml <- CM.liftIO getYAML
  commandsExp <- mkCommandsMonoid yaml
  [|subparser $(pure commandsExp)|]
  where
    mkCommandsMonoid :: [(String, String)] -> Q Exp
    mkCommandsMonoid [] =
      fail "mkCommandParserExp: empty command list (subparser would be useless)"
    mkCommandsMonoid (x : xs) =
      foldl
        (\accQ specQ -> [|$accQ <> $(mkOne specQ)|])
        (mkOne x)
        xs

    mkOne :: (String, String) -> Q Exp
    mkOne (nm, desc) =
      [|command nm (info (pure $ MyCommand nm) (progDesc desc))|]
