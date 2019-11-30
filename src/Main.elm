module Main exposing (..)

--Main code goes here

import Browser
-- import Html exposing (Html, Attribute, div, input, textarea, text, button)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onInput, onClick)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, placeholder, value, rows, cols)
import Html.Styled.Events exposing (onClick, onInput)
import Interpreter exposing (..)

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
     title = "Title!"
    , body = [ Html.Styled.toUnstyled (view model) ]
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({ model | content = newContent }, Cmd.none)
    Update -> ({ model | progOutput = simpleInterpret model.content }, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model = 
  div []
    [ 
      codingBlock model
    , div [
      css [
          centeredElements
        ]

    ] [ textarea [] [ text (model.progOutput) ] ]
    , button [ onClick Update ] [ text "blah" ]
    ]

theme : { secondary : Color, primary : Color, fontColor : Color }
theme =
    { primary = hex "002B36"
    , secondary = hex "2AA198" --rgb 250 240 230
    , fontColor = hex "FDF6E3"
    }

codingBlock : Model -> Html Msg
codingBlock model = textarea [ 
    placeholder "Brainfuck Program"
    , value model.content
    , onInput Change
    , rows 20
    , css [
        centeredElements
      , backgroundColor theme.primary
      , color theme.fontColor
      , fontSize (px 20)
      , width (pct 80)
      , borderColor theme.secondary
      , borderWidth (px 5)
      , padding (px 10)
    ]
  ] []

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