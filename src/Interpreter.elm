module Interpreter exposing (..)

import Dict exposing (Dict)
import Formatter exposing (convertFromOok)

-- Memory (pointer to current cell and "an array" of index to value)
type alias Memory = {
  pointer : Int
  , data : Dict Int Int
  }

-- Empty memory
defaultMemory : Memory
defaultMemory = {
  pointer = 0
  , data = Dict.empty
  }

type alias Program = {
    program: String
    {-- 
      A mapping of brackets to brackets, based on their character index. For example:
      "[[][]]" = 
      0 -> 5
      1 -> 2
      2 -> 1
      3 -> 4
      4 -> 3
      5 -> 0
    --}
    , jumpMap: Dict Int Int
    , instructionPointer : Int
    , output : List Char
    , inputChars : List Char
  }

-- Turns a String into a Program
createProgram : String -> String -> Program 
createProgram input progInput = {
    program = input
    , jumpMap = generateJumpMap input
    , instructionPointer = 0
    , output = []
    , inputChars = String.toList progInput
  }

-- Generates the jump map 
generateJumpMap : String -> Dict Int Int
generateJumpMap program = 
  let
    -- Reverses a dictionary
    doubleDict : Dict Int Int -> Dict Int Int
    doubleDict dict = List.foldl 
      (\elem -> \acc -> 
        case elem of 
          (k, v) -> Dict.insert v k acc) dict (Dict.toList dict)

    -- Converts a program into a jump map
    generateDict : String -> Int -> List Int -> Dict Int Int -> Dict Int Int
    generateDict prog pos stack dict = 
      case String.uncons prog of
        Just (head, tail) ->
          case head of
            '[' -> generateDict tail (pos + 1) (pos :: stack) dict
            ']' -> case stack of
              x::xs -> generateDict tail (pos + 1) xs (Dict.insert x pos dict)
              [] -> dict --This case should never occur!
            _ -> generateDict tail (pos + 1) stack dict
        Nothing -> dict
  in
    generateDict program 0 [] Dict.empty |> doubleDict

-- Prints the output of a program, given its memory
printOutput : (Program, Memory) -> String
printOutput result = 
  Tuple.first result 
  |> .output 
  |> String.fromList 
  |> String.reverse

-- Interprets a program string and returns its string output
simpleInterpret : String -> String -> String
simpleInterpret input progInput = 
  case validateProgram input progInput of
    Good -> interpret (createProgram (convertFromOok input) progInput) defaultMemory |> printOutput
    MismatchedBrackets -> "Failed to run program! (Some brackets aren't matching)"
    MissingInput -> "Failed to run program! (Input doesn't have enough characters)"

type Validation = Good | MismatchedBrackets | MissingInput

-- Checks if a program is valid (has the right number of brackets)
validateProgram : String -> String -> Validation
validateProgram program progInput = 
  let
    brackets : (Int, Int)
    brackets = countBrackets program

    countCommas : String -> Int
    countCommas str = 
      case String.uncons str of
        Just (x, xs) -> 1 + countCommas xs
        Nothing -> 0
  in
    if Tuple.first brackets /= Tuple.second brackets then MismatchedBrackets
    else if countCommas program > String.length progInput then MissingInput
    else Good

-- Counts the number of brackets in a String
countBrackets : String -> (Int, Int)
countBrackets str = 
  String.foldl (\c -> \(x, y) ->
    case c of 
      '[' -> (x + 1, y)
      ']' -> (x, y + 1)
      _ -> (x, y)) (0, 0) str

-- Given a program and its current memory, evaluate the program
-- to completion 
interpret : Program -> Memory -> (Program, Memory)
interpret program memory =
  let
      nextState = interpretInstruction program memory
      newProg = Tuple.first nextState
      newMem = Tuple.second nextState
  in
    if .instructionPointer newProg == (1 + String.length program.program)
    then (program, memory) 
    else interpret newProg newMem

-- Interpret a single instruction, given the program and its current memory
interpretInstruction : Program -> Memory -> (Program, Memory)
interpretInstruction program memory =
  let
    -- modify : (Int -> Int -> Int) -> (Int -> Int -> Int)
    -- modify operation = \index -> \cVal -> if index == memory.pointer then operation cVal 1 else cVal

    update : (Int -> Int -> Int) -> Int
    update operation = operation currentData 1

    currentData : Int
    currentData = Maybe.withDefault 0 <| Dict.get memory.pointer memory.data 

    programInstruction : Char
    programInstruction = 
      String.slice program.instructionPointer (program.instructionPointer + 1) program.program
      |> String.uncons
      |> Maybe.withDefault ('#', "")
      |> Tuple.first
  in
  
  case programInstruction of
    -- Increment pointer
    '>' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, { memory | pointer = memory.pointer + 1})

    -- Decrement pointer
    '<' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, { memory | pointer = memory.pointer - 1})

    -- Increment value at pointer
    '+' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, { memory | data = Dict.insert memory.pointer (update (+)) memory.data})

    -- Decrement value at pointer
    '-' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, { memory | data = Dict.insert memory.pointer (update (-)) memory.data})

    -- Output value at pointer
    '.' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      , output = Char.fromCode currentData :: program.output}, memory)

    -- Input value into pointer
    ',' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      , inputChars = Maybe.withDefault [] (List.tail program.inputChars)
      }, { memory | data = Dict.insert memory.pointer (Char.toCode <| Maybe.withDefault '\u{0000}' (List.head program.inputChars)) memory.data})

    -- Begin loop
    '[' -> case currentData of
      0 -> ({program 
        | instructionPointer = Maybe.withDefault 0 (Dict.get program.instructionPointer program.jumpMap)
        }, memory) -- Jump forward
      _ -> ({program 
        | instructionPointer = program.instructionPointer + 1
        }, memory)

    ']' -> case currentData of
      0 -> ({program 
        | instructionPointer = program.instructionPointer + 1
        }, memory)
      _ -> ({program 
        | instructionPointer = Maybe.withDefault 0 (Dict.get program.instructionPointer program.jumpMap)
        }, memory) -- Jump back

    _ -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, memory)