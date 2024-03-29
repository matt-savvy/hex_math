module HexMath exposing (Msg(..), cleanInput, main, update)

import Browser
import Css exposing (fontFamilies, monospace)
import Css.Global
import Hex exposing (fromHex, toHex)
import Html.Styled exposing (Html, div, form, h2, input, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, value)
import Html.Styled.Events exposing (onInput, onSubmit)
import Random
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw



-- MODEL


type alias Model =
    { valA : Int
    , valB : Int
    , input : String
    , answer : Maybe Answer
    , score : Int
    }


type Answer
    = Correct Int
    | Incorrect Int


type Msg
    = GotNewValue ( Int, Int )
    | GotInput String
    | GotSubmit


init : () -> ( Model, Cmd Msg )
init _ =
    ( { valA = 8, valB = 6, input = "", answer = Nothing, score = 0 }
    , generateValuesCommand
    )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Css.Global.global Tw.globalStyles
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.items_center
                , Tw.text_2xl
                , Breakpoints.lg [ Tw.text_5xl, Tw.gap_1 ]
                , Tw.w_full
                , Tw.h_screen
                , Tw.content_center
                , Tw.justify_around
                ]
            ]
            [ viewScore model.score
            , viewProblem ( model.valA, model.valB )
            , viewInput model.input
            , viewAnswer model.answer
            ]
        ]


formatHex : Int -> String
formatHex n =
    n |> toHex |> String.padLeft 2 '0'


viewScore : Int -> Html Msg
viewScore score =
    div []
        [ h2 [ css [ Tw.uppercase, gameFont ] ] [ text ("Score: " ++ formatHex score) ]
        , instructions
        ]


instructions : Html Msg
instructions =
    div [ css [ Tw.text_xl, Tw.text_center ] ] [ text "Add the hexidecimal numbers." ]


gameFont : Css.Style
gameFont =
    fontFamilies [ "courier", .value monospace ]


viewProblem : ( Int, Int ) -> Html Msg
viewProblem ( valA, valB ) =
    let
        styles =
            css [ Tw.w_fit, Tw.text_right, Tw.uppercase, gameFont, Tw.text_3xl, Breakpoints.lg [ Tw.text_6xl ] ]
    in
    div [ styles ]
        [ viewValue valA
        , div [] [ text "+", viewValue valB ]
        ]


viewValue : Int -> Html Msg
viewValue n =
    text (formatHex n)


viewInput : String -> Html Msg
viewInput inputStr =
    let
        styles =
            css
                [ Tw.uppercase
                , Tw.w_24
                , gameFont
                , Tw.text_2xl
                , Tw.tracking_widest
                , Tw.my_2
                , Tw.block
                , Tw.text_right
                , Tw.rounded_md
                , Tw.border_2
                , Tw.border_color Tw.gray_400
                , Tw.px_4
                , Tw.py_2
                , Tw.text_color Tw.gray_900
                ]
    in
    form [ onSubmit GotSubmit ]
        [ input [ autofocus True, onInput GotInput, value inputStr, styles ] []
        ]


viewAnswer : Maybe Answer -> Html Msg
viewAnswer maybeAnswer =
    let
        answerBody =
            case maybeAnswer of
                Just answer ->
                    text (answerStr answer)

                Nothing ->
                    text ""
    in
    div [ css [ Tw.h_24, Tw.uppercase, gameFont ] ] [ answerBody ]


answerStr : Answer -> String
answerStr answer =
    case answer of
        Correct solution ->
            formatHex solution ++ " is correct!"

        Incorrect solution ->
            "incorrect, answer is " ++ formatHex solution



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNewValue ( valA, valB ) ->
            ( { model | valA = valA, valB = valB }, Cmd.none )

        GotInput str ->
            ( { model | input = cleanInput str }, Cmd.none )

        GotSubmit ->
            if model.input == "" then
                ( model, Cmd.none )

            else
                let
                    answer =
                        fromHex model.input
                            |> Maybe.map
                                (getAnswer model.valA model.valB)
                in
                case answer of
                    Just (Correct _) ->
                        ( { model | answer = answer, input = "", score = model.score + 1 }, generateValuesCommand )

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
            Random.int 1 15
    in
    Random.generate GotNewValue (Random.pair randomHex randomHex)


getAnswer : Int -> Int -> Int -> Answer
getAnswer valA valB inputSolution =
    let
        solution =
            valA + valB
    in
    if solution == inputSolution then
        Correct solution

    else
        Incorrect solution



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        , update = update
        }
