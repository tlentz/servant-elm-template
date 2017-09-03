{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Example.Types
    ( Dice (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Elm (ElmType(..))
import GHC.Generics (Generic)

data Dice = Dice
    { numSides :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)