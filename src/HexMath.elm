module HexMath exposing (main)

import Browser
import Hex exposing (toHex)
import Html exposing (Html, div, h1, text)
import Html.Events exposing (..)
import Random



-- MODEL


type alias Model =
    { valA : Int, valB : Int }


type Msg
    = GotNewValue ( Int, Int )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { valA = 8, valB = 6 }
    , generateValuesCommand
    )


view : Model -> Html Msg
view model =
    div [] [ viewValue model.valA, viewValue model.valB ]


formatHex : Int -> String
formatHex n =
    n |> toHex |> String.padLeft 2 '0'


viewValue : Int -> Html Msg
viewValue n =
    h1 [] [ text (formatHex n) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNewValue ( valA, valB ) ->
            ( { model | valA = valA, valB = valB }, Cmd.none )


generateValuesCommand : Cmd Msg
generateValuesCommand =
    let
        randomHex : Random.Generator Int
        randomHex =
            Random.int 0 15
    in
    Random.generate GotNewValue (Random.pair randomHex randomHex)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
