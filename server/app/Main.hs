{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Api.Config.Types (Config(..), EnvVars(..))
import Api.Server (server)
import Api.Types (ApiWithAssets)
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Data.Bits (bit)
import Data.String.Conversions (cs)
import Database.PostgreSQL.Simple (connect, withTransaction)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Database.PostgreSQL.Simple.Migration
       (MigrationCommand(..), MigrationContext(..), runMigration)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy(..), serve)
import System.Envy (decodeEnv)
import System.Exit (die)

apiWithAssets :: Proxy ApiWithAssets
apiWithAssets = Proxy

app :: Application
app = serve apiWithAssets server

main :: IO ()
main = do
  let dir = "../db/migrate"
  config <- buildConfig
  -- comment this stuff out if you are not using postgres
  conn <- connect (postgresConfig config)
  withTransaction conn $ do
    runMigration $ MigrationContext MigrationInitialization True conn
    runMigration $ MigrationContext (MigrationDirectory dir) True conn
  -- end
  run (port config) app

buildPostgresConfig :: EnvVars -> ConnectInfo
buildPostgresConfig envVars =
  ConnectInfo
    (cs $ envPostgresHost envVars)
    (bit $ envPostgresPort envVars)
    (cs $ envPostgresUser envVars)
    (cs $ envPostgresPass envVars)
    (cs $ envPostgresDB envVars)

buildConfig :: IO Config
buildConfig = do
  envVars <- (decodeEnv :: IO (Either String EnvVars))
  eitherConfig <-
    runEitherT $ do
      envVars' <- hoistEither envVars
      let postgresConfig' = buildPostgresConfig envVars'
          port' = 3000
      return $ Config postgresConfig' port'
  case eitherConfig of
    (Left err) -> die err
    (Right config) -> return config
