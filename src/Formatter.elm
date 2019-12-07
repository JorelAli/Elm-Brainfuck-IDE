module Formatter exposing (format, convertFromOok, convertToOok, unformat)

format : String -> String
format program = 
  if isOok program then program else 
    formatIterator '#' 0 (strip program) "" |> cleanup

cleanup : String -> String
cleanup semiFormatted =
  String.replace " ," ","
  <| String.replace " ." "."
  <| String.replace "\n\n" "\n"
  <| String.replace ">>>>>" ">>>>> "
  <| String.replace "<<<<<" "<<<<< "
  <| String.replace "-----" "----- " 
  <| String.replace "+++++" "+++++ " 
  <| semiFormatted

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

formatChar : Int -> Char -> Char -> Char -> (String, Int)
formatChar indentation prev cur next =
  let
    indentationToString : Int -> String
    indentationToString level = 
      case level of 
        0 -> ""
        _ -> "    " ++ indentationToString (level - 1)
  in
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
unformat str = if isOok str then str else strip str

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

isOok : String -> Bool
isOok = String.contains "Ook" 

convertToOok : String -> String
convertToOok str = 
  let

    convertToOokHelper : String -> String -> String
    convertToOokHelper restOfProgram acc =
      case String.uncons restOfProgram of
        Just (letter, tail) -> 
          case letter of
            '>' -> convertToOokHelper tail (acc ++ "Ook. Ook? ")
            '<' -> convertToOokHelper tail (acc ++ "Ook? Ook. ")
            '+' -> convertToOokHelper tail (acc ++ "Ook. Ook. ")
            '-' -> convertToOokHelper tail (acc ++ "Ook! Ook! ")
            '.' -> convertToOokHelper tail (acc ++ "Ook! Ook. ")
            ',' -> convertToOokHelper tail (acc ++ "Ook. Ook! ")
            '[' -> convertToOokHelper tail (acc ++ "Ook! Ook? ")
            ']' -> convertToOokHelper tail (acc ++ "Ook? Ook! ")
            _ -> convertToOokHelper tail acc
        Nothing -> acc
  in
    if isOok str then str else 
      convertToOokHelper (strip str) ""

convertFromOok : String -> String
convertFromOok str = 
  let

    listOfOoks : List String
    listOfOoks = String.words str

    pairList : List String -> List (String, String)
    pairList list = 
      case list of
        a :: b :: xs -> (a, b) :: pairList xs
        [] -> []
        [_] -> []
    
    ookToBf : (String, String) -> String
    ookToBf pair = 
      case pair of
        ("Ook.", "Ook?") -> ">"
        ("Ook?", "Ook.") -> "<"
        ("Ook.", "Ook.") -> "+"
        ("Ook!", "Ook!") -> "-"
        ("Ook!", "Ook.") -> "."
        ("Ook.", "Ook!") -> ","
        ("Ook!", "Ook?") -> "["
        ("Ook?", "Ook!") -> "]"
        _ -> ""
  in
    if not <| isOok str then str else 
      String.join "" <| List.map ookToBf (listOfOoks |> pairList)