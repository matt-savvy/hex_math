module HexMathTest exposing (..)

import Expect
import Fuzz exposing (..)
import HexMath exposing (..)
import Test exposing (..)


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
        ]
