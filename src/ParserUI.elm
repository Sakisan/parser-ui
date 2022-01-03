module ParserUI exposing (deadEndsToString)

import Parser exposing (..)


deadEndsToString : List DeadEnd -> String
deadEndsToString list =
    List.map deadEndToString list
        |> String.join "\n"


deadEndToString : DeadEnd -> String
deadEndToString deadEnd =
    ("row: " ++ String.fromInt deadEnd.row)
        ++ (" col: " ++ String.fromInt deadEnd.col ++ " ")
        ++ (case deadEnd.problem of
                Expecting string ->
                    "Expecting: " ++ string

                ExpectingInt ->
                    "ExpectingInt"

                ExpectingHex ->
                    "ExpectingHex"

                ExpectingOctal ->
                    "ExpectingOctal"

                ExpectingBinary ->
                    "ExpectingBinary"

                ExpectingFloat ->
                    "ExpectingFloat"

                ExpectingNumber ->
                    "ExpectingNumber"

                ExpectingVariable ->
                    "ExpectingVariable"

                ExpectingSymbol string ->
                    "ExpectingSymbol: " ++ string

                ExpectingKeyword string ->
                    "ExpectingKeyword: " ++ string

                ExpectingEnd ->
                    "ExpectingEnd"

                UnexpectedChar ->
                    "UnexpectedChar"

                Problem string ->
                    "Problem: " ++ string

                BadRepeat ->
                    "BadRepeat"
           )
