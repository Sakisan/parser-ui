module ParserUI exposing (Split, deadEndsToString, splitDeadEnds)

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


type alias Split =
    { location : String
    , before : String
    , after : String
    , expected : String
    }


splitDeadEnds : String -> List DeadEnd -> List Split
splitDeadEnds input deadEnds =
    List.map (splitDeadEnd input) deadEnds


splitDeadEnd : String -> DeadEnd -> Split
splitDeadEnd input deadEnd =
    let
        line =
            getAt (deadEnd.row - 1) (String.lines input)
                |> Maybe.withDefault input
    in
    { location =
        "row "
            ++ String.fromInt deadEnd.row
            ++ " col "
            ++ String.fromInt deadEnd.col
    , before = String.left (deadEnd.col - 1) line
    , after = String.dropLeft (deadEnd.col - 1) line
    , expected =
        case deadEnd.problem of
            Expecting string ->
                string

            ExpectingSymbol string ->
                string

            ExpectingKeyword string ->
                string

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

            ExpectingEnd ->
                "ExpectingEnd"

            UnexpectedChar ->
                "UnexpectedChar"

            Problem string ->
                string

            BadRepeat ->
                "BadRepeat"
    }


{-| Returns `Just` the element at the given index in the list,
or `Nothing` if the index is out of range.
-}
getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs
