module Types exposing (..)

import Generated.ApiTypes exposing (Dice)
import Http


type Msg
    = Roll
    | NewFace Int
    | DiceRollFailure Http.Error


type alias Model =
    { dice : Dice
    , dieFace : Int
    }


initialModel : Model
initialModel =
    { dice = initialDice
    , dieFace = 1
    }


initialDice : Dice
initialDice =
    { numSides = 6
    }
