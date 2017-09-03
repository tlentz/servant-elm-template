{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds         #-}

module Main where
import Data.Proxy (Proxy (Proxy))
import Elm (Spec (Spec), specsToDir, toElmTypeSource, toElmDecoderSource, toElmEncoderSource)
import Servant.Elm (ElmOptions (..), defElmImports, defElmOptions, generateElmForAPIWith, UrlPrefix (Static))
import Api.Types
import Data.Text
import Api.Example.Types (Dice(..))

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "/servant-elm-template" }


specs :: [Spec]
specs =
  [ 
    Spec ["Shared", "Generated"]
         ( defElmImports `append` "import Dict exposing (Dict)\n"
         : toElmTypeSource (Proxy :: Proxy Dice)
         : toElmDecoderSource (Proxy :: Proxy Dice)
         : toElmEncoderSource (Proxy :: Proxy Dice)
         : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api)
         )
  ]

main :: IO ()
main = specsToDir specs "frontend/elm"