module Hex exposing (fromHex, toHex)


toHex : Int -> String
toHex n =
    let
        exponent =
            n
                |> toFloat
                |> logBase 16
                |> floor
                |> max 0

        digits =
            toHexDigits [] n exponent
    in
    digits
        |> List.reverse
        |> List.map toHexVal
        |> String.join ""


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


fromHex : String -> Maybe Int
fromHex str =
    str
        |> String.toUpper
        |> String.toList
        |> List.reverse
        |> fromHexHelper 0 0


fromHexHelper : Int -> Int -> List Char -> Maybe Int
fromHexHelper exp sum lst =
    case lst of
        [] ->
            Just sum

        head :: rest ->
            hexVal head
                |> Maybe.andThen
                    (\val ->
                        fromHexHelper (exp + 1) (sum + (val * 16 ^ exp)) rest
                    )


codePointA : Int
codePointA =
    65


codePointF : Int
codePointF =
    71


codePoint0 : Int
codePoint0 =
    48


hexVal : Char -> Maybe Int
hexVal char =
    if Char.isHexDigit char then
        let
            code =
                Char.toCode char
        in
        if code >= codePointA && code <= codePointF then
            Just (code - codePointA + 10)

        else
            Just (code - codePoint0)

    else
        Nothing
