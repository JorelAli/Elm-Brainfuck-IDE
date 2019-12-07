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
import Html.Styled exposing (Html, div, textarea, button, text, hr, p, input)
import Html.Styled.Attributes exposing (css, href, src, placeholder, value, rows, cols, readonly, attribute, type_)
import Html.Styled.Events exposing (onClick, onInput)

import Dict

-- Brainfuck helpers
import Interpreter exposing (simpleInterpret, interpretWithMemory, Memory, Bits(..))
import Formatter exposing (format, unformat, convertToOok, convertFromOok)

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
> +++."""

-- Color schemes / "constant" CSS values
theme : { 
    secondary : Color
  , primary : Color
  , fontColor : Color
  , fontSize : Float
  , margins : Px
  , primaryStr : String
  , secondaryStr : String 
  }
theme =
  let 
    primaryStr = "360036" 
    secondaryStr = "660066"
  in
  { primary = hex primaryStr
  , secondary = hex secondaryStr
  , fontColor = hex "FDF6E3"
  , fontSize = 16
  , margins = (px 20)
  , primaryStr = primaryStr
  , secondaryStr = secondaryStr
  }

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
subscriptions model = Sub.none

-- MODEL
type alias Model = { 
      code : String         -- The Brainfuck code 
    , progOutput : String   -- Brainfuck output
    , progInput : String    -- Brainfuck inputs
    , displaySidebar : Bool -- Whether sidebar is enabled
    , autoRun : Bool
    , numBits : Bits
  }

-- Initial state
init : flags -> (Model, Cmd Msg)
init _ = ({ 
    code = defaultProgram  
  , progOutput = ""        
  , progInput = ""         
  , displaySidebar = False 
  , autoRun = False
  , numBits = Eight
  }, Cmd.none)

-- UPDATE
type Msg
  = EditCode String  -- Update the program's code
  | Execute          -- Evaluates the program
  | Format           -- Formats the code
  | Unformat         -- Unformats the code
  | GotoGithub       -- Goes to GitHub
  | ToggleSidebar    -- Toggles the sidebar
  | ConvertToOok     -- Converts Brainfuck to Ook
  | ConvertFromOok   -- Converts from Ook to Brainfuck
  | EditInput String -- When the input to the program has been changed
  | ToggleAutoRun
  | UpdateBits Bits

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditCode newCode    -> ({ model | code = newCode, progOutput = if model.autoRun then simpleInterpret newCode model.progInput model.numBits else model.progOutput }, Cmd.none)
    Format              -> ({ model | code = format model.code }, Cmd.none)
    Unformat            -> ({ model | code = unformat model.code }, Cmd.none)
    ConvertToOok        -> ({ model | code = convertToOok model.code }, Cmd.none)
    ConvertFromOok      -> ({ model | code = convertFromOok model.code }, Cmd.none)

    GotoGithub          -> (model, load "https://github.com/JorelAli/Elm-Brainfuck-IDE")

    ToggleSidebar       -> ({ model | displaySidebar = not model.displaySidebar }, Cmd.none)
    EditInput newInput  -> ({ model | progInput = newInput }, Cmd.none)
    Execute             -> ({ model | progOutput = simpleInterpret model.code model.progInput model.numBits }, Cmd.none)
    ToggleAutoRun       -> ({ model | autoRun = not model.autoRun, progOutput = simpleInterpret model.code model.progInput model.numBits }, Cmd.none)
    UpdateBits bits     -> ({ model | numBits = bits }, Cmd.none)

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
      , if model.displaySidebar then optionCodeBlock model else codingBlock model
      , inputBlock model
      , outputBlock model 
      , memoryBlock model
    ]

-- Title (at top of page)
title : Html Msg
title = div [
    css [
      fontSize (pt 40)
      , color theme.fontColor
      , centeredElements
      , textAlign center
      , paddingTop (px 40)
      , width (pct 100)
      , fontFamilies [ "monospace" ]
    ]
  ] 
  [ 
    text "Brainfuck IDE" 
    , hr [
      css [
        marginTop zero
      ]
    ] []
  ]

-- Option code block
optionCodeBlock : Model -> Html Msg
optionCodeBlock model = 
  let
    optionButtonCss : Html.Styled.Attribute msg
    optionButtonCss = css [ 
        buttonCss 
        , width (pct 100) 
        , height (pt 40)
        , marginLeft zero
        , marginBottom theme.margins
      ]
    
    labelOf : String -> Html msg
    labelOf str = p [ css [labelCss] ] [ text str ]

    rule : Html msg
    rule = hr [] []

    radioAttrs : Bits -> List (Html.Styled.Attribute Msg)
    radioAttrs bits = [
      css [ 
        buttonCss 
        , width (pct 25) 
        , height (pt 40)
        , marginLeft zero
        , if bits == model.numBits then backgroundColor theme.secondary else backgroundColor theme.primary
        ]
      , onClick (UpdateBits bits)
      ]

  in
  div [
    css [
      width (pct 100)
      , displayFlex
    ]
  ] [
    textarea [ 
      placeholder "Brainfuck Program"
      , value model.code
      , onInput EditCode
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
      ]
    ] []
  , div [
      css [
        width (pct 20)
        , marginLeft theme.margins
      ]
    ] [
      labelOf "Ook Conversion", rule
      , button [ optionButtonCss, onClick ConvertToOok ] [ text "Convert to Ook!" ] 
      , button [ optionButtonCss, onClick ConvertFromOok ] [ text "Convert from Ook!" ] 
      , labelOf "Code Formatting", rule
      , button [ optionButtonCss, onClick Format ] [ text "Format code" ] 
      , button [ optionButtonCss, onClick Unformat ] [ text "Minify code" ] 
      , labelOf "External Links", rule
      , button [ optionButtonCss, onClick GotoGithub ] [ text "GitHub Page" ] 
      , labelOf "Number of Bits", rule
      , div [ css [ displayFlex ] ] [
        button (radioAttrs Eight) [ text "8" ] 
        , button (radioAttrs Sixteen) [ text "16" ] 
        , button (radioAttrs ThirtyTwo) [ text "32" ] 
        , button (radioAttrs Unlimited) [ text "±∞" ] 
        ]
    ]
  ]

inputBlock : Model -> Html Msg
inputBlock model =  div [
    css [
      width (pct 100)
      , displayFlex
      , marginBottom theme.margins
      , marginTop theme.margins
    ]
  ] [ 
    textarea [
    css [
        centeredElements
      , backgroundColor theme.primary
      , color theme.fontColor
      , fontSize (pt theme.fontSize)
      , width (pct 100)
      , borderColor theme.secondary
      , borderWidth (px 5)
      , padding (px 10)
      , height (pt 20)
      , minHeight (pt 20)
      , overflowY hidden
    ]
    , placeholder "Code input"
    , onInput EditInput
  ] []
  ]

memoryBlock : Model -> Html Msg
memoryBlock model = 
  let
    result : (String, Memory)
    result = interpretWithMemory model.code model.progInput model.numBits

    memoryCellCss : Style
    memoryCellCss = Css.batch [
        width (calc (pct 5) minus (px 30)) 
        , backgroundColor theme.primary
        , color theme.fontColor
        , fontSize (pt theme.fontSize)
        , borderColor theme.secondary
        , borderWidth (px 5)
        , borderStyle solid
        , fontFamilies ["monospace"]
        , padding (px 10)
        , textAlign center
      ]

    generateBlocks : Memory -> List (Html Msg)
    generateBlocks memory =
      Dict.foldr (\k -> \v -> \acc -> 
        div [
          css [
            memoryCellCss
            , backgroundColor (if memory.pointer == k then theme.secondary else theme.primary)
          ]
        ] [ text (String.fromInt v) ] :: acc
      ) [] (memory.data) 

  in
    div [
      css [
      width (pct 100)
      , displayFlex
      , flexWrap wrap
      , marginBottom theme.margins
      , marginTop theme.margins
      ]
    ] (generateBlocks (Tuple.second result))

-- Output block (where program output goes)
outputBlock : Model -> Html Msg
outputBlock model = div [
    css [
      width (pct 100)
      , displayFlex
      , marginBottom theme.margins
      , marginTop theme.margins
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
      , onClick Execute 
    ] [ text "Run code!" ] 
  , button [ 
      css [ 
        buttonCss 
        , width (em 3)
        , backgroundColor (if model.displaySidebar then theme.secondary else theme.primary)
      ]
      , onClick ToggleSidebar 
    ] [ text "🛠" ] 
  , button [ 
      css [ 
        buttonCss 
        , width (em 3)
        , backgroundColor (if model.autoRun then theme.secondary else theme.primary)
      ]
      , onClick ToggleAutoRun 
    ] [ text "▶️" ] 
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
    , fontFamilies ["monospace"]
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
    , fontFamilies ["monospace"]
    , marginTop zero
    , marginBottom zero
  ]

-- Main coding block
codingBlock : Model -> Html Msg
codingBlock model = div []
  [
    textarea [ 
      placeholder "Brainfuck Program"
      , value model.code
      , onInput EditCode
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