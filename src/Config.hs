{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..)
  , defaultConfig
  , readConfig
  )
  where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON (..), (.:))
import GHC.Word

data Config = Config
  { cDbUser     :: !String 
  , cDbPassword :: !String
  , cDbHost     :: !String
  , cDbPort     :: !Word16
  , cDbDatabase :: !String
  } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
     v .: "db-user" <*>
     v .: "db-password" <*>
     v .: "db-host" <*>
     v .: "db-port" <*>
     v .: "db-database"

defaultConfig = Config
  "pubmed"
  "pubmed"
  "localhost"
  5432
  "pubmed2"

readConfig :: String -> IO (Either Y.ParseException Config)
readConfig = Y.decodeFileEither 

-- >>> readConfig "hsrelex-import.yaml"
-- Right (Config {cDbUser = "pubmed", cDbPassword = "pubmed", cDbHost = "localhost", cDbPort = 5432, cDbDatabase = "pubmed3"})
