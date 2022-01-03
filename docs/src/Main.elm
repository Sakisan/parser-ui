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
    let
        code1 =
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
    Element.column [ Element.spacing 15 ]
        [ Element.el [] (Element.text "What debugging a parser looks like now:")
        , codePreview code1 (Element.html Wip.Main1.main)
        , Element.el [] (Element.text "Ah yes, the notoriously missing deadEndsToString.")
        , Element.el [] (Element.text "A straight forward implementation at least gives us this:")
        , codePreview """
                    case Parser.run intParser input of
                        Ok parsed ->
                            String.fromInt parsed

                        Err e ->
                            ParserUI.deadEndsToString e
                      """
            (Element.html Wip.Main2.main)
        , Element.el [] (Element.text "Still this is not very practical when dealing with bigger inputs and bigger parsers.")
        ]


codePreview : String -> Element msg -> Element msg
codePreview code preview =
    Element.column [ Element.spacing 15 ]
        [ SyntaxHighlight.elm code
            |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
            |> Result.withDefault (Html.pre [] [ Html.code [] [ Html.text code ] ])
            |> Element.html
            |> Element.el
                [ Border.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
                , Border.color Style.darkGrey
                , Border.dashed
                , Background.color Style.codeBackgroundColor
                ]
        , preview
            |> Element.el
                [ Border.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
                , Border.color Style.darkGrey
                , Border.dotted
                , Style.fillWidth
                , Background.color Style.codeBackgroundColor
                ]
        ]
