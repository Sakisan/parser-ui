module Wip.Main2 exposing (main)

import Html exposing (Html)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import ParserUI exposing (..)


intParser : Parser Int
intParser =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


main : Html msg
main =
    let
        viewParse input =
            Html.li []
                [ Html.text <|
                    case Parser.run intParser input of
                        Ok parsed ->
                            String.fromInt parsed

                        Err e ->
                            ParserUI.deadEndsToString e
                ]
    in
    Html.ul [] (List.map viewParse [ "5", "-2", "0.0", "blurp" ])
