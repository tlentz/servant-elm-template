{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where
import Data.Proxy (Proxy (Proxy))
import Elm (Spec (Spec), specsToDir, toElmTypeSource, toElmDecoderSource, toElmEncoderSource)
import Servant.Elm (ElmOptions (..), defElmImports, defElmOptions, generateElmForAPIWith, UrlPrefix (Static))
import Api.Types
import Data.Text as DT
import Api.Example.Types (Dice(..))

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "/servant-elm-template" }

elmImports :: [Text]
elmImports = [ "import Dict exposing (Dict)"
              ]

specs :: [Spec]
specs =
  [ 
    Spec ["Generated", "ApiTypes"]
         ( defElmImports `append` (DT.intercalate "\n" elmImports)
         : toElmTypeSource (Proxy :: Proxy Dice)
         : toElmDecoderSource (Proxy :: Proxy Dice)
         : toElmEncoderSource (Proxy :: Proxy Dice)
         : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api)
         )
  ]

main :: IO ()
main = specsToDir specs "../client/elm"