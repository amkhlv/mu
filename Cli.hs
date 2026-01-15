{-# LANGUAGE TemplateHaskell #-}

module Cli where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Yaml as Yaml
import qualified Control.Monad.IO.Class as CM
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import System.Directory (getHomeDirectory)

data Actions = Actions [(String, Either Actions String)]

processObject :: Aeson.Object -> Actions
processObject obj = Actions
  [ (T.unpack (Key.toText key), valueToString value)
  | (key, value) <- KM.toList obj
  ]
  where
    valueToString :: Aeson.Value -> Either Actions String
    valueToString (Aeson.String s) = Right $ T.unpack s
    valueToString (Aeson.Object obj) = Left $ processObject obj
    valueToString _                = Left (Actions [])

getYAML :: String -> IO Actions
getYAML f = do
  yaml <- Yaml.decodeFileEither f
  case yaml of
    Right value -> return $ processObject value
    Left err -> fail $ "YAML Error: " ++ Yaml.prettyPrintParseException err

data MyCommand = MyCommand String deriving (Show)

mkCommandParserExp :: TH.Q TH.Exp
mkCommandParserExp  = do
  hm <- THS.runIO getHomeDirectory
  let cfg = hm ++ "/.config/amkhlv/mu.yaml"
  THS.addDependentFile cfg
  yaml <- THS.runIO $ getYAML cfg
  commandsExp <- mkCommandsMonoid yaml
  [|hsubparser $(pure commandsExp)|]
  where
    mkCommandsMonoid :: Actions -> TH.Q TH.Exp
    mkCommandsMonoid (Actions []) =
      fail "mkCommandParserExp: empty command list (subparser would be useless)"
    mkCommandsMonoid (Actions (x : xs)) =
      foldl
        (\accQ specQ -> [|$accQ <> $(mkOne specQ)|])
        (mkOne x)
        xs

    mkOne :: (String, Either Actions String) -> TH.Q TH.Exp
    mkOne (nm, Right cmd) =
      [|command nm (info (pure $ MyCommand cmd) (progDesc nm))|]
    mkOne (nm, Left actions) = do
      rst <- mkCommandsMonoid actions
      [|command nm (info (hsubparser $(pure rst)) (progDesc nm))|]
