import Html exposing (..)
import Html.Events exposing (..)
import Types exposing (Msg(..), Model, initialModel, initialDice)
import Rest exposing (rollDice)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, rollDice initialDice)

    NewFace newFace ->
      (Model model.dice newFace, Cmd.none)
    
    DiceRollFailure err ->
      (model, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString model.dieFace) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]