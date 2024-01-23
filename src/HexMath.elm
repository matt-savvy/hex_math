module HexMath exposing (cleanInput, main)

import Browser
import Hex exposing (fromHex, toHex)
import Html exposing (Html, div, form, h1, h2, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (..)
import Random



-- MODEL


type alias Answer =
    ( Bool, Int )


type alias Model =
    { valA : Int, valB : Int, input : String, answer : Maybe Answer }


type Msg
    = GotNewValue ( Int, Int )
    | GotInput String
    | GotSubmit


init : () -> ( Model, Cmd Msg )
init _ =
    ( { valA = 8, valB = 6, input = "", answer = Nothing }
    , generateValuesCommand
    )


view : Model -> Html Msg
view model =
    div []
        [ viewValue model.valA
        , viewValue model.valB
        , viewInput model.input
        , viewAnswer model.answer
        ]


formatHex : Int -> String
formatHex n =
    n |> toHex |> String.padLeft 2 '0'


viewValue : Int -> Html Msg
viewValue n =
    h1 [] [ text (formatHex n) ]


viewInput : String -> Html Msg
viewInput inputStr =
    form [ onSubmit GotSubmit ]
        [ input [ onInput GotInput, value inputStr ] []
        ]


viewAnswer : Maybe Answer -> Html Msg
viewAnswer maybeAnswer =
    case maybeAnswer of
        Just answer ->
            h2 [] [ text (answerStr answer) ]

        Nothing ->
            text ""


answerStr : Answer -> String
answerStr ( correct, solution ) =
    let
        solutionHex =
            toHex solution
    in
    if correct then
        solutionHex ++ " is correct!"

    else
        "incorrect, answer is " ++ solutionHex



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

        GotSubmit ->
            let
                answer =
                    fromHex model.input
                        |> Maybe.map
                            (getAnswer model.valA model.valB)
            in
            case answer of
                Just ( True, _ ) ->
                    ( { model | answer = answer, input = "" }, generateValuesCommand )

                _ ->
                    ( { model | answer = answer, input = "" }, Cmd.none )


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


getAnswer : Int -> Int -> Int -> Answer
getAnswer valA valB inputSolution =
    let
        solution =
            valA + valB
    in
    ( solution == inputSolution, solution )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
