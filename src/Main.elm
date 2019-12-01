module Main exposing (..)

--Main code goes here

-- Import browser 
import Browser

-- Subscription handling
import Browser.Events
import Json.Decode as Decode

-- CSS
import Css exposing (..)
import Css.Global exposing (global, body, selector)

-- HTML
import Html.Styled exposing (Html, div, textarea, button, text, hr)
import Html.Styled.Attributes exposing (css, href, src, placeholder, value, rows, cols, readonly, attribute)
import Html.Styled.Events exposing (onClick, onInput)

-- Brainfuck helpers
import Interpreter exposing (simpleInterpret)
import Formatter exposing (format)

-- CONSTANTS
defaultProgram : String
defaultProgram = """+++++ +++++ 
[
    > +++
    > +++++ ++
    > +++++ +++++ 
    <<< -
]
>> ++.
> +++++ .
<< +++.
"""

-- MAIN
main : Platform.Program () Model Msg
-- Note the use of Browser.document, which gives us 
-- access to a much more powerful "view" method
main = Browser.document { 
  init = init
  , update = update
  , view = \model -> {
      -- Title for webpage
      title = "Brainfuck IDE"
      -- Set global body styling
    , body = List.map Html.Styled.toUnstyled [
      global [ 
        body [
          backgroundColor theme.background
        ]
        -- Style the scrollbar with Webkit
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
      -- Rest of the HTML
      , view model
    ]
  }
  , subscriptions = subscriptions
  }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none -- Browser.Events.onKeyDown keyDecoder

-- keyDecoder : Decode.Decoder Msg
-- keyDecoder = Decode.map toKey (Decode.field "key" Decode.string)

-- toKey : String -> Msg
-- toKey string =
--   case string of 
--     "Tab" -> EnterTab
--     _ -> DoNothing

-- MODEL
type alias Model = { 
      content : String    -- Program input
    , progOutput : String -- Program output
  }

-- Initial state
init : flags -> (Model, Cmd Msg)
init _ = ({ content = defaultProgram, progOutput = "" }, Cmd.none)

-- UPDATE
type Msg
  = Change String -- When the program input has been changed
  | Update        -- When a program has been evaluated (run code is pressed)
  | UpdateFormat  -- When the format button is pressed
  -- | EnterTab
  -- | DoNothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- Update program input
    Change newContent ->
      ({ model | content = newContent }, Cmd.none)
    -- Update program output
    Update -> ({ model | progOutput = simpleInterpret model.content }, Cmd.none)
    -- Format program
    UpdateFormat -> ({ model | content = format model.content }, Cmd.none)
    -- DoNothing -> (model, Cmd.none)
    -- EnterTab -> ({model | content = ""}, Cmd.none)

-- VIEW (Main div)
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

-- Color schemes / "constant" CSS values
theme : { secondary : Color, background : Color, fontColor : Color, fontSize : Float }
theme =
  { background = hex "002B36"
  , secondary = hex "2AA198"
  , fontColor = hex "FDF6E3"
  , fontSize = 16
  }

-- Title (at top of page)
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

-- Output block (where program output goes)
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

-- CSS for buttons
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

-- Toolbar of useful buttons (below the output)
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

-- Main coding block
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

-- Style that centers elements
centeredElements : Style
centeredElements = Css.batch [ 
    display block
    , resize vertical
    , marginLeft auto
    , marginRight auto
    , width (pct 50)
  ]