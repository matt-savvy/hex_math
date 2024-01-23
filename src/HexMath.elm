module HexMath exposing (cleanInput, main)

import Browser
import Hex exposing (toHex)
import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (..)
import Random



-- MODEL


type alias Model =
    { valA : Int, valB : Int, input : String }


type Msg
    = GotNewValue ( Int, Int )
    | GotInput String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { valA = 8, valB = 6, input = "" }
    , generateValuesCommand
    )


view : Model -> Html Msg
view model =
    div []
        [ viewValue model.valA
        , viewValue model.valB
        , viewInput model.input
        ]


formatHex : Int -> String
formatHex n =
    n |> toHex |> String.padLeft 2 '0'


viewValue : Int -> Html Msg
viewValue n =
    h1 [] [ text (formatHex n) ]


viewInput : String -> Html Msg
viewInput inputStr =
    input [ onInput GotInput, value inputStr ] []



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

        GotInput str ->
            ( { model | input = cleanInput str }, Cmd.none )


cleanInput : String -> String
cleanInput input =
    input
        |> String.filter Char.isHexDigit
        |> String.toUpper


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
