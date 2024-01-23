module HexTest exposing (..)

import Expect
import Fuzz exposing (..)
import Hex exposing (fromHex, toHex)
import Test exposing (..)


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
            , test "10" <|
                \_ -> Expect.equal "A" (toHex 10)
            , test "11" <|
                \_ -> Expect.equal "B" (toHex 11)
            , test "12" <|
                \_ -> Expect.equal "C" (toHex 12)
            , test "13" <|
                \_ -> Expect.equal "D" (toHex 13)
            , test "14" <|
                \_ -> Expect.equal "E" (toHex 14)
            , test "15" <|
                \_ -> Expect.equal "F" (toHex 15)
            , test "24" <|
                \_ -> Expect.equal "18" (toHex 24)
            , test "538" <|
                \_ -> Expect.equal "21A" (toHex 538)
            ]
        , describe "fromHex"
            [ fuzz (intRange 0 9) "0-9" <|
                \n -> Expect.equal (fromHex (String.fromInt n)) (Just n)
            , test "F" <|
                \_ -> Expect.equal (fromHex "F") (Just 15)
            , test "0F" <|
                \_ -> Expect.equal (fromHex "0F") (Just 15)
            , test "3B" <|
                \_ -> Expect.equal (fromHex "3B") (Just 59)
            , test "206" <|
                \_ -> Expect.equal (fromHex "206") (Just 518)
            ]
        ]
