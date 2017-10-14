module Rest exposing (..)

import Generated.ApiTypes exposing (Dice, postApiRollDice)
import Http
import Types exposing (Msg(..))


rollDice : Dice -> Cmd Msg
rollDice dice =
    Http.send processResult <| postApiRollDice dice


processResult : Result Http.Error Int -> Msg
processResult result =
    case result of
        Ok num ->
            NewFace num

        Err err ->
            DiceRollFailure err
