module Style exposing (..)

import Element exposing (Element)
import Element.Font as Font



-- STYLE


white =
    Element.rgb 1 1 1


black =
    Element.rgb 0 0 0


lightGrey =
    Element.rgb255 238 238 238


grey =
    Element.rgb255 178 178 178


darkGrey =
    Element.rgb255 128 128 128


blue =
    Element.rgb255 0x2B 0x4E 0x72


red =
    Element.rgb 1 0.25 0.25


pageBackgroundColor =
    Element.rgb255 0x26 0x26 0x26


codeBackgroundColor =
    -- Element.rgb255 0x20 0x20 0x20
    pageBackgroundColor


pageTextColor =
    Element.rgb255 0xDC 0xD3 0xB1


linkColor =
    Element.rgb255 0xF0 0x50 0x33


linkHoverColor =
    Element.rgb255 0xDD 0x11 0x44


fillWidth =
    Element.width Element.fill


justBold text =
    Element.el [ Font.bold ] (Element.text text)
