{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Config.Types
  ( Config(..)
  , EnvVars(..)
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import System.Envy (FromEnv, (.!=), envMaybe, fromEnv)

data Config = Config
  { postgresConfig :: ConnectInfo
  , port :: Int
  } deriving (Show)

data EnvVars = EnvVars
  { envPostgresHost :: Text
  , envPostgresPort :: Int
  , envPostgresUser :: Text
  , envPostgresPass :: Text
  , envPostgresDB :: Text
  } deriving (Show)

instance FromEnv EnvVars where
  fromEnv =
    EnvVars <$> envMaybe "POSTGRES_HOST" .!= "localHost" <*>
    envMaybe "POSTGRES_PORT" .!= 5432 <*>
    envMaybe "POSTGRES_USER" .!= "postgres" <*>
    envMaybe "POSTGRES_PASS" .!= "postgres" <*>
    envMaybe "POSTGRES_DATABASE" .!= "example"
