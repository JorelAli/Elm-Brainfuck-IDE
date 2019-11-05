module Main exposing (..)

--Main code goes here

import Browser
import Html exposing (Html, Attribute, div, input, textarea, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Interpreter exposing (..)

-- CONSTANTS
defaultProgram : String
defaultProgram = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

-- MAIN
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
view model =
  div []
    [ textarea [ placeholder "Brainfuck Program", value model.content, onInput Change, rows 43, cols 80 ] []
    , div [] [ textarea [] [ text (model.progOutput) ] ]
    , button [ onClick Update ] [ text "blah" ]
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