module Hex exposing (toHex)


toHex : Int -> String
toHex n =
    let
        exponent =
            highestExponent n 0

        digits =
            toHexDigits [] n exponent
    in
    digits
        |> List.reverse
        |> List.map toHexVal
        |> String.join ""


highestExponent : Int -> Int -> Int
highestExponent n exp =
    let
        values =
            n // (16 ^ exp)
    in
    if values < 16 then
        exp

    else
        highestExponent n (exp + 1)


toHexDigits : List Int -> Int -> Int -> List Int
toHexDigits acc n exp =
    if exp >= 0 then
        let
            values =
                n // (16 ^ exp)
        in
        toHexDigits (values :: acc) (modBy (16 ^ exp) n) (exp - 1)

    else
        acc


toHexVal : Int -> String
toHexVal n =
    if n >= 0 && n < 10 then
        String.fromInt n

    else
        (65 + (n - 10))
            |> Char.fromCode
            |> String.fromChar
