{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Types
    ( Api
    , ApiWithAssets
    ) where

import Servant ((:<|>), (:>), Post, JSON, ReqBody, Raw)
import Api.Example.Types (Dice)

type Api
        = "api"
            :> ("rollDice" :> ReqBody '[JSON] Dice
                           :> Post '[JSON] Int
               )

type ApiWithAssets = "servant-elm-template" :> (Api :<|> Raw)