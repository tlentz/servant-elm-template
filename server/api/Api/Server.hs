{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.Example.Handler (rollDice)

server :: Server ApiWithAssets
server = rollDice' :<|> serveStatic'
    where
        rollDice' = rollDice
        serveStatic' = serveDirectoryFileServer "../client/dist"