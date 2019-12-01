module Main exposing (..)

--Main code goes here

import Browser
-- import Html exposing (Html, Attribute, div, input, textarea, text, button)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onInput, onClick)
import Css exposing (..)
import Css.Global exposing (global, body, selector)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, value, rows, cols, readonly)
import Html.Styled.Events exposing (onClick, onInput)
import Interpreter exposing (..)
import Formatter exposing (format)

-- CONSTANTS
defaultProgram : String
defaultProgram = """+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set 70/100/30/10
    > +++++ ++              add  7 to cell #1
    > +++++ +++++           add 10 to cell #2
    > +++                   add  3 to cell #3
    > +                     add  1 to cell #4
<<<< -                  decrement counter (cell #0)
]
> ++ .                  print 'H'
> + .                   print 'e'
+++++ ++ .              print 'l'
.                       print 'l'
+++ .                   print 'o'
> ++ .                  print ' '
<< +++++ +++++ +++++ .  print 'W'
> .                     print 'o'
+++ .                   print 'r'
----- - .               print 'l'
----- --- .             print 'd'
> + .                   print '!'
> .                     print '\n'
"""

-- MAIN
main : Platform.Program () Model Msg
main = Browser.document { 
  init = init
  , update = update
  , view = \model -> {
     title = "Brainfuck IDE"
    , body = List.map Html.Styled.toUnstyled [
      global [ 
        body [
          backgroundColor theme.background
        ]
        , selector "::-webkit-scrollbar" [
          property "background" "#002B36"
        ]
        , selector "::-webkit-scrollbar-track" [
          property "background" "#002B36"
        ]
        , selector "::-webkit-scrollbar-thumb" [
          property "background" "#2AA198"
        ]
        , selector "::-webkit-scrollbar-thumb:hover" [
          property "background" "#2AA198"
        ]
        , selector "::-webkit-resizer" [
          property "background-color" "#2AA198"
          , property "background" "#2AA198"
          , property "color" "#2AA198"
        ]
      ]
      , (view model) 
    ]
  }
  , subscriptions = \model -> Sub.none
  }

-- MODEL
type alias Model = { 
    content : String
    , progOutput : String
  }

init : flags -> (Model, Cmd Msg)
init _ = ({ content = defaultProgram, progOutput = "" }, Cmd.none)

-- UPDATE
type Msg
  = Change String
  | Update
  | UpdateFormat

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({ model | content = newContent }, Cmd.none)
    Update -> ({ model | progOutput = simpleInterpret model.content }, Cmd.none)
    UpdateFormat -> ({ model | content = format model.content }, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = 
  div [
    css [
      marginLeft auto
      , marginRight auto
      , width (pct 80)
    ]
  ]
    [ 
      title
      , codingBlock model
      , outputBlock model 
      , toolbar model
    ]

theme : { secondary : Color, background : Color, fontColor : Color, fontSize : Float }
theme =
    { background = hex "002B36"
    , secondary = hex "2AA198" --rgb 250 240 230
    , fontColor = hex "FDF6E3"
    , fontSize = 16
    }

title : Html Msg
title = div [
    css [
      fontSize (pt 40)
      , color theme.fontColor
      , centeredElements
      , textAlign center
      , paddingBottom (px 20)
      , paddingTop (px 20)
      , width (pct 100)
      , fontFamilies [ "monospace" ]
    ]
  ] 
  [ 
    text "Brainfuck IDE" 
    , hr [] []
  ]

outputBlock : Model -> Html Msg
outputBlock model = div [
    css [
      width (pct 100)
      , displayFlex
      , marginBottom (px 20)
    ]
  ] [ 
    textarea [
    css [
        centeredElements
      , backgroundColor theme.background
      , color theme.fontColor
      , fontSize (pt theme.fontSize)
      , width (pct 80)
      , borderColor theme.secondary
      , borderWidth (px 5)
      , padding (px 10)
      , height (pt 20)
      , minHeight (pt 20)
      , overflowY hidden
    ]
    , readonly True
  ] [ text (model.progOutput) ]
  , button [ 
      css [ buttonCss ]
      , onClick Update 
    ] [ text "Run code!" ] 
  ]

buttonCss : Style
buttonCss = Css.batch [
    width (pct 20)
    , marginLeft (px 20)
    , backgroundColor theme.background
    , color theme.fontColor
    , fontSize (pt theme.fontSize)
    , borderColor theme.secondary
    , borderWidth (px 5)
    , borderStyle solid
    , hover
      [ 
        borderColor theme.background
        , backgroundColor theme.secondary
      ]
  ]

toolbar : Model -> Html Msg
toolbar model = div [
    css [
      width (pct 100)
      , displayFlex
      , marginBottom (px 20)
    ]
  ] [ 
    button [ 
      css [
        centeredElements
        , buttonCss
        , height (pt 40)
        , width (pct 25)
        , marginLeft zero
      ]
      , onClick UpdateFormat 
    ] [ text "Format code" ] 
    , button [ 
      css [
        centeredElements
        , buttonCss
        , height (pt 40)
        , width (pct 25)
      ]
      , onClick Update 
    ] [ text "Run code!" ] 
    , button [ 
      css [
        centeredElements
        , buttonCss
        , height (pt 40)
        , width (pct 25)
      ]
      , onClick Update 
    ] [ text "Run code!" ] 
    , button [ 
      css [
        centeredElements
        , buttonCss
        , height (pt 40)
        , width (pct 25)
      ]
      , onClick Update 
    ] [ text "Run code!" ] 
  ]

codingBlock : Model -> Html Msg
codingBlock model = div []
  [
    textarea [ 
      placeholder "Brainfuck Program"
      , value model.content
      , onInput Change
      , rows 20
      , css [
          centeredElements
        , backgroundColor theme.background
        , color theme.fontColor
        , fontSize (pt theme.fontSize)
        , width (calc (pct 100) minus (px 30))
        , borderColor theme.secondary
        , borderWidth (px 5)
        , padding (px 10)
        , marginBottom (px 20)
      ]
    ] []
  ]

centeredElements : Style
centeredElements = Css.batch [ 
    display block
    , resize vertical
    , marginLeft auto
    , marginRight auto
    , width (pct 50)
  ]

simpleInterpret : String -> String
simpleInterpret input = if
    validateProgram input == False
  then
    "nope"
  else 
    interpret (createProgram input) defaultMemory |> printOutput

validateProgram : String -> Bool
validateProgram program = 
  let
    brackets : (Int, Int)
    brackets = countBrackets program
  in
    Tuple.first brackets == Tuple.second brackets