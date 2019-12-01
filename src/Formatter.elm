module Formatter exposing (..)

format : String -> String
format program = formatIterator '#' 0 (strip program) "" |> cleanup

cleanup : String -> String
cleanup semiFormatted =
  let
    a = String.replace "+++++" "+++++ " semiFormatted
    b = String.replace "-----" "----- " a
    c = String.replace "<<<<<" "<<<<< " b
    d = String.replace ">>>>>" ">>>>> " c
    e = String.replace "\n\n" "\n" d
    f = String.replace " ." "." e
    g = String.replace " ," "," f
  in 
    g

formatIterator : Char -> Int -> String -> String -> String
formatIterator prevChar indentation restOfProgram currentOutput = 
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
    result = formatChar indentation prevChar currentChar nextChar
  in
    case currentChar of
      '#' -> currentOutput
      _ ->
        formatIterator currentChar (Tuple.second result) (
          case String.uncons restOfProgram of
            Just (x, xs) -> xs
            Nothing -> restOfProgram) (currentOutput ++ Tuple.first result)
  
indentationToString : Int -> String
indentationToString level = 
  case level of 
    0 -> ""
    _ -> "    " ++ indentationToString (level - 1)

formatChar : Int -> Char -> Char -> Char -> (String, Int)
formatChar indentation prev cur next =
  case cur of
    '>' -> case prev of
      '>' -> (">", indentation)
      '<' -> (">", indentation)
      '#' -> (indentationToString indentation ++ ">", indentation)
      _ -> ("\n" ++ indentationToString indentation ++ ">", indentation)
    '<' -> case prev of
      '>' -> ("<", indentation)
      '<' -> ("<", indentation)
      '#' -> (indentationToString indentation ++ "<", indentation)
      _ -> ("\n" ++ indentationToString indentation ++ "<", indentation)
    '+' -> if prev == '<' || prev == '>'
      then (" +", indentation)
      else ("+", indentation)
    '-' -> if prev == '<' || prev == '>'
      then (" -", indentation)
      else ("-", indentation)
    '.' -> (indentationToString indentation ++ ".\n", indentation)
    ',' -> (indentationToString indentation ++ ",\n", indentation)
    '[' -> if next == '-' || next == '+'
      then ("\n" ++ indentationToString indentation ++ "[\n" ++ indentationToString (indentation + 1), indentation + 1) 
      else ("\n" ++ indentationToString indentation ++ "[", indentation + 1) 
    ']' -> if next == '-' || next == '+'
      then ("\n" ++ indentationToString (indentation - 1) ++ "]\n" ++ indentationToString (indentation - 1), indentation - 1)
      else ("\n" ++ indentationToString (indentation - 1) ++ "]", indentation - 1)
    _ -> ("", indentation)

unformat : String -> String 
unformat = strip

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