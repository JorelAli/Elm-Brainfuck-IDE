module Main exposing (..)

--Main code goes here

import Browser
-- import Html exposing (Html, Attribute, div, input, textarea, text, button)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onInput, onClick)
import Interpreter exposing (..)

import Html exposing (Html)

import Element exposing (Element, el, text, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

-- CONSTANTS
defaultProgram : String
defaultProgram = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

-- MAIN
-- main : Html msg
main = 
  
  Browser.sandbox { 
      init = init
      , update = update
      , view = view
      }

-- MODEL
type alias Model = { 
    content : String
    , progOutput : String
  }

init : Model
init =
  { content = defaultProgram, progOutput = "" }

-- UPDATE
type Msg
  = Change String
  | Update

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    Update -> { model | progOutput = simpleInterpret model.content }

-- VIEW
view : Model -> Html Msg
view model = Element.layout [] myRowOfStuff 
  -- div []
  --   [ textarea [ placeholder "Brainfuck Program", value model.content, onInput Change, rows 43, cols 80 ] []
  --   , div [] [ textarea [] [ text (model.progOutput) ] ]
  --   , button [ onClick Update ] [ text "blah" ]
  --   ]



myRowOfStuff : Element msg
myRowOfStuff =
    row [ width fill, centerY, spacing 30 ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text "stylish!")

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