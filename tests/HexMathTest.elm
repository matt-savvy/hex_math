module HexMathTest exposing (..)

import Expect
import Fuzz exposing (..)
import HexMath exposing (..)
import Test exposing (..)


baseModel =
    { valA = 1, valB = 2, input = "", answer = Nothing, score = 0 }


suite : Test
suite =
    describe "HexMath"
        [ describe "cleanInput"
            [ fuzz string "is all hexDigit" <|
                \input ->
                    Expect.equal True
                        (String.all Char.isHexDigit (cleanInput input))
            , fuzz string "is all uppercase" <|
                \input ->
                    Expect.equal True
                        (String.all (\char -> Char.isUpper char || Char.isDigit char) (cleanInput input))
            ]
        , describe "update"
            [ test "GotSubmit with empty string is no op" <|
                \_ ->
                    let
                        model =
                            { baseModel | input = "" }
                    in
                    Expect.equal ( model, Cmd.none ) (update GotSubmit model)
            ]
        ]
