module Main exposing (..)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Style
import SyntaxHighlight
import Wip.Main1
import Wip.Main2
import Wip.Main3


main : Html msg
main =
    Element.layout [] <|
        Element.column
            [ Style.fillWidth
            , Element.spacing 15
            , Font.family [ Font.typeface "Ubuntu", Font.typeface "Menlo" ]
            , Font.color Style.pageTextColor
            , Background.color Style.pageBackgroundColor
            ]
            [ viewHeader
            , Element.el [ Element.padding 10 ] body
            , viewFooter
            ]


viewHeader : Element msg
viewHeader =
    Element.row
        [ Element.height (Element.px 56)
        , Style.fillWidth
        , Element.centerY
        , Element.spacing 16
        , Element.padding 8
        ]
        [ Element.el [ Element.padding 15 ] (Element.text "ParserUI explained")
        ]


viewFooter : Element msg
viewFooter =
    Element.row
        [ Element.height (Element.px 72)
        , Style.fillWidth
        , Element.centerY
        , Element.spacing 5
        , Element.padding 10
        ]
        [ Element.el [ Element.alignRight ] (Element.text "produced to you by Antoine Snyers")
        ]


body : Element msg
body =
    Element.column
        [ Element.spacing 15
        , Element.width (Element.maximum 1200 Element.fill)
        ]
    <|
        List.map (\el -> Element.el [ Element.width Element.fill ] el)
            [ text "What debugging a parser looks like now:"
            , let
                code =
                    """
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
                            Parser.deadEndsToString e
                ]
    in
    Html.ul [] (List.map viewParse [ "5", "-2", "0.0", "blurp" ])
"""
              in
              codePreview code (Element.html Wip.Main1.main)
            , text "Ah yes, the notoriously missing deadEndsToString."
            , text "A straight forward implementation at least gives us this:"
            , codePreview """ParserUI.deadEndsToString e"""
                (Element.html Wip.Main2.main)
            , text "Still this is not very practical when dealing with bigger inputs and bigger parsers. So let's bring them in and see what we can do with them."
            , spaceUp <| text "This example input is from advent of code 2020 day 7"
            , inputView Wip.Main3.input
            , text "We want to parse this into a fairly unsophisticated list of bags."
            , codeBlock """
type alias Color =               -- "light red bags"
    String

type alias Contents =            -- "1 bright white bag, 2 muted yellow bags."
    List ( Int, Color )

type alias BagCollection =       -- This should have each line of the input parsed
    List ( Color, Contents )
"""
            , text "A succesful parser would give us this:"
            , showPreview Wip.Main3.main_2
            , text "This parser fails however"
            , let
                code =
                    """
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
    Parser.succeed (\\a b -> a ++ " " ++ b)
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
                """
              in
              codePreview code (Element.html Wip.Main3.main)
            , spaceUp <| text "Let's try again with a little more visualization."
            , showPreview (Element.html Wip.Main3.main_1b)
            , text "This error was rendered using ParserUI.splitDeadEnds"
            , codeBlock """
main : Html msg
main =
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
        ] """
            , spaceUp <| text "Another way to render the same error with elm-ui"
            , showPreview Wip.Main3.main_1c
            , codeBlock """
main : Element msg
main =
    case Parser.run parser input of
        Ok parsed ->
            Element.el [] (Element.text "parsed succesfully")

        Err list ->
            Element.column
                [ Element.spacing 20 ]
                (list
                    |> List.sortBy (\\d -> negate d.col)
                    |> ParserUI.splitDeadEnds input
                    |> List.map viewSplit
                )


viewSplit : Split -> Element msg
viewSplit split =
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
        ] """
            , spaceUp <| text "By the way the fix for the parser is as follows"
            , codeBlock
                """
    -- replace this line
    |. Parser.oneOf [ Parser.token "bag", Parser.token "bags" ]

    -- by this line
    |. Parser.oneOf [ Parser.token "bags", Parser.token "bag" ]"""
            ]


text str =
    Element.paragraph [] [ Element.el [] (Element.text str) ]


codePreview : String -> Element msg -> Element msg
codePreview code preview =
    Element.column [ Element.spacing 15 ]
        [ codeBlock code
        , showPreview preview
        ]


showPreview : Element msg -> Element msg
showPreview preview =
    Element.el
        [ Border.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
        , Border.color Style.darkGrey
        , Border.dotted
        , Element.padding 10
        , Background.color Style.codeBackgroundColor
        ]
        preview
        |> indent


codeBlock : String -> Element msg
codeBlock code =
    SyntaxHighlight.elm code
        |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
        |> Result.withDefault (Html.pre [] [ Html.code [] [ Html.text code ] ])
        |> Element.html
        |> Element.el
            [ Border.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
            , Border.color Style.darkGrey
            , Border.dashed
            , Element.padding 10
            , Background.color Style.codeBackgroundColor
            ]
        |> indent


inputView : String -> Element msg
inputView input =
    Element.el
        [ Element.padding 10
        , Border.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
        , Border.color Style.darkGrey
        , Element.padding 10
        , Background.color Style.codeBackgroundColor
        ]
        (Element.text input)
        |> indent


indent =
    Element.el [ Element.paddingXY 30 0 ]


spaceUp =
    Element.el [ Element.paddingEach { top = 50, right = 0, left = 0, bottom = 0 } ]
