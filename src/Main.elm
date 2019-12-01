module Main exposing (..)

--Main code goes here

-- Import browser 
import Browser

-- Subscription handling
import Browser.Events
import Json.Decode as Decode

-- Command handling
import Browser.Navigation exposing (load)

-- CSS
import Css exposing (..)
import Css.Global exposing (global, body, selector)

-- HTML
import Html.Styled exposing (Html, div, textarea, button, text, hr, p)
import Html.Styled.Attributes exposing (css, href, src, placeholder, value, rows, cols, readonly, attribute)
import Html.Styled.Events exposing (onClick, onInput)

-- Brainfuck helpers
import Interpreter exposing (simpleInterpret)
import Formatter exposing (format, unformat)

-- CONSTANTS
defaultProgram : String
defaultProgram = """+++++ +++++ 
[
    > +++++ ++
    > +++++ +++++
    > +++ 
    <<< -
]
> ++.
> +++++.
> +++.
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
          backgroundColor theme.primary
        ]
        -- Style the scrollbar with Webkit
        , selector "::-webkit-scrollbar" [
          property "background" ("#" ++ theme.primaryStr)
        ]
        , selector "::-webkit-scrollbar-track" [
          property "background" ("#" ++ theme.primaryStr)
        ]
        , selector "::-webkit-scrollbar-thumb" [
          property "background" ("#" ++ theme.secondaryStr)
        ]
        , selector "::-webkit-scrollbar-thumb:hover" [
          property "background" ("#" ++ theme.secondaryStr)
        ]
        , selector "::-webkit-resizer" [
          property "background-color" ("#" ++ theme.secondaryStr)
          , property "background" ("#" ++ theme.secondaryStr)
          , property "color" ("#" ++ theme.secondaryStr)
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
    , displayOptions : Bool 
  }

-- Initial state
init : flags -> (Model, Cmd Msg)
init _ = ({ content = defaultProgram, progOutput = "", displayOptions = True }, Cmd.none)

-- UPDATE
type Msg
  = Change String -- When the program input has been changed
  | Update        -- When a program has been evaluated (run code is pressed)
  | UpdateFormat  -- When the format button is pressed
  | Unformat      -- Unformats the code
  | GotoGithub    -- ... Goes to GitHub
  | ToggleOptions 
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
    Unformat -> ({ model | content = unformat model.content }, Cmd.none)
    GotoGithub -> (model, load "https://github.com/JorelAli/Elm-Brainfuck-IDE")
    ToggleOptions -> ({ model | displayOptions = not model.displayOptions }, Cmd.none)
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
      , if model.displayOptions then optionCodeBlock model else codingBlock model
      , outputBlock model 
      , toolbar model
    ]

-- Color schemes / "constant" CSS values
theme : { secondary : Color, primary : Color, fontColor : Color, fontSize : Float, margins : Px, primaryStr : String, secondaryStr : String }
theme =
  let 
    primaryStr = "360036" 
    secondaryStr = "660066"
  in
  { primary = hex primaryStr --"002B36" 
  , secondary = hex secondaryStr --"2AA198"
  , fontColor = hex "FDF6E3"
  , fontSize = 16
  , margins = (px 20)
  , primaryStr = primaryStr
  , secondaryStr = secondaryStr
  }

-- Title (at top of page)
title : Html Msg
title = div [
    css [
      fontSize (pt 40)
      , color theme.fontColor
      , centeredElements
      , textAlign center
      , paddingBottom theme.margins
      , paddingTop (px 40)
      , width (pct 100)
      , fontFamilies [ "monospace" ]
    ]
  ] 
  [ 
    text "Brainfuck IDE" 
    , hr [] []
  ]

optionCodeBlock : Model -> Html Msg
optionCodeBlock model = div [
    css [
      width (pct 100)
      , displayFlex
    ]
  ] [
    textarea [ 
      placeholder "Brainfuck Program"
      , value model.content
      , onInput Change
      , rows 20
      , css [
          centeredElements
        , backgroundColor theme.primary
        , color theme.fontColor
        , fontSize (pt theme.fontSize)
        , width (calc (pct 80) minus (px 30))
        , borderColor theme.secondary
        , borderWidth (px 5)
        , padding (px 10)
        , minHeight (pt 20)
        , marginBottom theme.margins
      ]
    ] []
  , div [
      css [
        width (pct 20)
        , marginLeft theme.margins
      ]
    ] [
      p [ css [labelCss] ] [ text "Ook converter" ]
      , hr [] []
      , button [ 
        css [ 
          buttonCss 
          , width (pct 100) 
          , marginLeft zero
          , marginBottom theme.margins
        ]
        , onClick Update 
        ] [ text "Convert to Ook!" ] 
      , button [ 
        css [ 
          buttonCss 
          , width (pct 100) 
          , marginLeft zero
          , marginBottom theme.margins
        ]
        , onClick Update 
        ] [ text "Convert from Ook!" ] 
      , p [ css [labelCss] ] [ text "Option here" ]
      , hr [] []
      , button [ 
        css [ 
          buttonCss 
          , width (pct 100) 
          , marginLeft zero
        ]
        , onClick Update 
        ] [ text "Run code!" ] 
    ]
  ]

-- Output block (where program output goes)
outputBlock : Model -> Html Msg
outputBlock model = div [
    css [
      width (pct 100)
      , displayFlex
      , marginBottom theme.margins
    ]
  ] [ 
    textarea [
    css [
        centeredElements
      , backgroundColor theme.primary
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
    , placeholder "Code output"
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
    , marginLeft theme.margins
    , backgroundColor theme.primary
    , color theme.fontColor
    , fontSize (pt theme.fontSize)
    , borderColor theme.secondary
    , borderWidth (px 5)
    , borderStyle solid
    , hover
      [ 
        backgroundColor theme.secondary
      ]
  ]

-- CSS for labels
labelCss : Style
labelCss = Css.batch [
    width (pct 100)
    , backgroundColor theme.primary
    , color theme.fontColor
    , fontSize (pt theme.fontSize)
    , textAlign center
    , fontFamilies ["Arial"]
    , marginTop zero
    , marginBottom zero
  ]

-- Toolbar of useful buttons (below the output)
toolbar : Model -> Html Msg
toolbar model = div [
    css [
      width (pct 100)
      , displayFlex
      , marginBottom theme.margins
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
      , onClick Unformat 
    ] [ text "Minify code" ] 
    , button [ 
      css [
        centeredElements
        , buttonCss
        , height (pt 40)
        , width (pct 25)
      ]
      , onClick ToggleOptions
    ] [ text "Toggle option panel" ] 
    , button [ 
      css [
        centeredElements
        , buttonCss
        , height (pt 40)
        , width (pct 25)
      ]
      , onClick GotoGithub 
    ] [ text "GitHub page" ] 
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
        , backgroundColor theme.primary
        , color theme.fontColor
        , fontSize (pt theme.fontSize)
        , width (calc (pct 100) minus (px 30))
        , borderColor theme.secondary
        , borderWidth (px 5)
        , padding (px 10)
        , marginBottom theme.margins
        , minHeight (pt 20)
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