module Shared.Generated exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Dict exposing (Dict)

type alias Dice =
    { numSides : Int
    }

decodeDice : Decoder Dice
decodeDice =
    decode Dice
        |> required "numSides" int

encodeDice : Dice -> Json.Encode.Value
encodeDice x =
    Json.Encode.object
        [ ( "numSides", Json.Encode.int x.numSides )
        ]

postApiRollDice : Dice -> Http.Request (Int)
postApiRollDice body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "/servant-elm-template"
                , "api"
                , "rollDice"
                ]
        , body =
            Http.jsonBody (encodeDice body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }