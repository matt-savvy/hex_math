module HexTest exposing (..)

import Expect
import Fuzz exposing (..)
import Hex exposing (fromHex, toHex)
import Test exposing (..)


maxSafe : Int
maxSafe =
    (2 ^ 31) - 1


suite : Test
suite =
    describe "Hex"
        [ describe "toHex"
            [ fuzz (intRange 0 9) "0-9" <|
                \n -> Expect.equal (toHex n) (String.fromInt n)
            , fuzz (intRange 0 9) "0-15 is one digit" <|
                \n -> Expect.equal (String.length (toHex n)) 1
            , fuzz (intRange 16 255) "16-255 is two digits" <|
                \n -> Expect.equal (String.length (toHex n)) 2
            , fuzz (intRange 256 4095) "256-4095 is three digits" <|
                \n -> Expect.equal (String.length (toHex n)) 3
            , test "0x10" <|
                \_ -> Expect.equal "A" (toHex 0x0A)
            , test "0x11" <|
                \_ -> Expect.equal "B" (toHex 0x0B)
            , test "0x12" <|
                \_ -> Expect.equal "C" (toHex 0x0C)
            , test "0x13" <|
                \_ -> Expect.equal "D" (toHex 0x0D)
            , test "0x14" <|
                \_ -> Expect.equal "E" (toHex 0x0E)
            , test "0x15" <|
                \_ -> Expect.equal "F" (toHex 0x0F)
            , test "0x18" <|
                \_ -> Expect.equal "18" (toHex 0x18)
            , test "0x21A" <|
                \_ -> Expect.equal "21A" (toHex 0x021A)
            , test "max safe" <|
                \_ -> Expect.equal "7FFFFFFF" (toHex maxSafe)
            , fuzz (intRange 0 maxSafe) "is all hex digits" <|
                \n ->
                    Expect.equal True
                        (String.all Char.isHexDigit (toHex n))
            ]
        , describe "fromHex"
            [ fuzz (intRange 0 9) "0-9" <|
                \n -> Expect.equal (fromHex (String.fromInt n)) (Just n)
            , test "0xF" <|
                \_ -> Expect.equal (fromHex "F") (Just 0x0F)
            , test "0x0F" <|
                \_ -> Expect.equal (fromHex "0F") (Just 0x0F)
            , test "0x3B" <|
                \_ -> Expect.equal (fromHex "3B") (Just 0x3B)
            , test "0x206" <|
                \_ -> Expect.equal (fromHex "206") (Just 0x0206)
            ]
        ]
