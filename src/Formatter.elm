module Formatter exposing (..)

format : String -> String
format program = formatIterator '#' (strip program) ""

formatIterator : Char -> String -> String -> String
formatIterator prevChar restOfProgram currentOutput = 
  let
    currentChar = 
      case String.uncons restOfProgram of 
        Just (letter, rest) -> letter
        Nothing -> '#'
    nextChar =     
      case String.uncons restOfProgram of 
        Just (letter, rest) -> 
          case String.uncons rest of 
            Just (letter2, rest2) -> letter2
            Nothing -> '#'
        Nothing -> '#'
  in
    case currentChar of
      '#' -> currentOutput
      _ ->
        formatIterator currentChar (
          case String.uncons restOfProgram of
            Just (x, xs) -> xs
            Nothing -> restOfProgram) (currentOutput ++ formatChar prevChar currentChar nextChar)
  
formatChar : Char -> Char -> Char -> String
formatChar prev cur next =
  case cur of
    '>' -> case prev of
      '>' -> ">"
      '<' -> ">"
      '#' -> "    >"
      _ -> "\n" ++ "    " ++ ">"
    '<' -> case prev of
      '>' -> "<"
      '<' -> "<"
      '#' -> "    <"
      _ -> "\n    <"
    '+' -> if prev == '<' || prev == '>'
      then " +"
      else "+"
    '-' -> if prev == '<' || prev == '>'
      then " -"
      else "-"
    '.' -> "    ."
    ',' -> "    ,"
    '[' -> "\n    ["
    ']' -> "\n    ]"
    _ -> ""

strip : String -> String
strip str = 
  case String.uncons str of
    Just (letter, rest) -> if
      letter == '+'
      || letter == '-'
      || letter == '>'
      || letter == '<'
      || letter == '['
      || letter == ']'
      || letter == '.'
      || letter == ',' then 
        String.cons letter (strip rest) else (strip rest)
    Nothing -> ""