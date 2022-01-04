module Wip.Main3 exposing (input, main, main_1b, main_1c, main_2)

import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import ParserUI exposing (..)


input =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"""


type alias Color =
    String


type alias Contents =
    List ( Int, Color )


type alias BagCollection =
    List ( Color, Contents )



-- _1


parser : Parser BagCollection
parser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = lineParser
        , trailing = Optional
        }


lineParser : Parser ( Color, Contents )
lineParser =
    Parser.succeed Tuple.pair
        |= bagParser
        |. Parser.spaces
        |. Parser.token "contain"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed []
                |. Parser.token "no other bags"
            , Parser.sequence
                { start = ""
                , separator = ","
                , end = ""
                , spaces = Parser.chompWhile ((==) ' ')
                , item = nBagsParser
                , trailing = Forbidden
                }
            ]
        |. Parser.symbol "."


bagParser : Parser Color
bagParser =
    Parser.succeed (\a b -> a ++ " " ++ b)
        |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
        |. Parser.spaces
        |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
        |. Parser.spaces
        |. Parser.oneOf [ Parser.token "bag", Parser.token "bags" ]


nBagsParser : Parser ( Int, String )
nBagsParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.spaces
        |= bagParser


main : Html msg
main =
    Html.text <|
        case Parser.run parser input of
            Ok parsed ->
                "parsed succesfully"

            Err e ->
                ParserUI.deadEndsToString e


main_1b : Html msg
main_1b =
    case Parser.run parser input of
        Ok parsed ->
            Html.text "parsed succesfully"

        Err e ->
            Html.ul [] <| List.map viewSplit (ParserUI.splitDeadEnds input e)


viewSplit : Split -> Html msg
viewSplit split =
    Html.li []
        [ Html.p [] [ Html.text ("at: " ++ split.location) ]
        , Html.p [] [ Html.text ("Expected: " ++ split.before), Html.u [] [ Html.text split.expected ] ]
        , Html.p [] [ Html.text ("But got:  " ++ split.before ++ split.after) ]
        ]


main_1c : Element msg
main_1c =
    case Parser.run parser input of
        Ok parsed ->
            Element.el [] (Element.text "parsed succesfully")

        Err list ->
            Element.column
                [ Element.spacing 20 ]
                (list
                    |> List.sortBy (\d -> negate d.col)
                    |> ParserUI.splitDeadEnds input
                    |> List.map viewSplit_1c
                )


viewSplit_1c : Split -> Element msg
viewSplit_1c split =
    Element.column []
        [ Element.el [] (Element.text split.location)
        , Element.paragraph
            []
            [ Element.el []
                (Element.text split.before)
            , Element.el [ Font.color (Element.rgb 1 0.25 0.25) ]
                (Element.text split.expected)
            ]
        , split.after
            |> String.left 100
            |> Element.text
            |> Element.el [ Font.color (Element.rgb255 128 128 128) ]
        ]



-- _2


main_2 : Element msg
main_2 =
    Element.el [] <|
        Element.text <|
            case Parser.run parser_2 input of
                Ok parsed ->
                    let
                        viewContents ( i, c ) =
                            "(" ++ String.fromInt i ++ ", \"" ++ c ++ "\")"

                        viewCollection =
                            parsed
                                |> List.map
                                    (\( color, contents ) ->
                                        "(\""
                                            ++ color
                                            ++ "\", ["
                                            ++ String.join "," (List.map viewContents contents)
                                            ++ "])"
                                    )
                                |> String.join "\n , "
                    in
                    " [ " ++ viewCollection ++ "\n ]"

                Err e ->
                    ParserUI.deadEndsToString e


parser_2 : Parser BagCollection
parser_2 =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = lineParser_2
        , trailing = Optional
        }


lineParser_2 : Parser ( Color, Contents )
lineParser_2 =
    Parser.succeed Tuple.pair
        |= bagParser_2
        |. Parser.spaces
        |. Parser.token "contain"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed []
                |. Parser.token "no other bags"
            , Parser.sequence
                { start = ""
                , separator = ","
                , end = ""
                , spaces = Parser.chompWhile ((==) ' ')
                , item = nBagsParser_2
                , trailing = Forbidden
                }
            ]
        |. Parser.symbol "."


bagParser_2 : Parser Color
bagParser_2 =
    Parser.succeed (\a b -> a ++ " " ++ b)
        |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
        |. Parser.spaces
        |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
        |. Parser.spaces
        |. Parser.oneOf [ Parser.token "bags", Parser.token "bag" ]


nBagsParser_2 : Parser ( Int, String )
nBagsParser_2 =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.spaces
        |= bagParser_2
