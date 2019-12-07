module Interpreter exposing (..)

import Dict exposing (Dict)
import Formatter exposing (convertFromOok, unformat)

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

type Bits = Eight | Sixteen | ThirtyTwo | Unlimited

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
    , numBits : Bits
  }

-- Turns a String into a Program
createProgram : String -> String -> Bits -> Program 
createProgram input progInput bits = {
    program = input
    , jumpMap = generateJumpMap input
    , instructionPointer = 0
    , output = []
    , inputChars = String.toList progInput
    , numBits = bits
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
simpleInterpret : String -> String -> Bits -> String
simpleInterpret input progInput bits = 
  case validateProgram input progInput of
    Good -> interpret (createProgram (convertFromOok input) progInput bits) defaultMemory |> printOutput
    MismatchedBrackets -> "Failed to run program! (Some brackets aren't matching)"
    MissingInput -> "Failed to run program! (Input doesn't have enough characters)"
    InfiniteLoop -> "Infinite loop detected! (Look for [] in your code)"

interpretWithMemory : String -> String -> Bits -> (String, Memory)
interpretWithMemory input progInput bits =
  case validateProgram input progInput of
    Good -> 
      let 
        result : (Program, Memory)
        result = interpret (createProgram (convertFromOok input) progInput bits) defaultMemory
      in
        (result |> printOutput, Tuple.second result)
    MismatchedBrackets -> ("Failed to run program! (Some brackets aren't matching)", defaultMemory)
    MissingInput -> ("Failed to run program! (Input doesn't have enough characters)", defaultMemory)
    InfiniteLoop -> ("Infinite loop detected! (Look for [] in your code)", defaultMemory)

type Validation = Good | MismatchedBrackets | MissingInput | InfiniteLoop

-- Checks if a program is valid (has the right number of brackets)
validateProgram : String -> String -> Validation
validateProgram program progInput = 
  let
    brackets : (Int, Int)
    brackets = countBrackets program

    countCommas : String -> Int
    countCommas str = 
      case String.uncons str of
        Just (x, xs) -> if x == ',' then 1 + countCommas xs else countCommas xs
        Nothing -> 0

    checkInfiniteLoops : String -> Bool
    checkInfiniteLoops str = String.contains "[]" (unformat str)
  in
    if Tuple.first brackets /= Tuple.second brackets then MismatchedBrackets
    else if checkInfiniteLoops program then InfiniteLoop
    else if countCommas program > String.length progInput then Good --MissingInput
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

    sanitizeInt : Bits -> Int -> Int
    sanitizeInt bits val = 
      case bits of
        Eight -> modBy (2^8) val
        Sixteen  -> modBy (2^16) val
        ThirtyTwo  -> modBy (2^32) val
        Unlimited -> val

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
      }, { memory | pointer = memory.pointer + 1
                    , data = if Dict.member (memory.pointer + 1) memory.data then memory.data else Dict.insert (memory.pointer + 1) 0 memory.data }) 

    -- Decrement pointer
    '<' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, { memory | pointer = if memory.pointer == 0 then 0 else memory.pointer - 1
                    , data = if Dict.member (if memory.pointer == 0 then 0 else memory.pointer - 1) memory.data then memory.data else Dict.insert (memory.pointer - 1) 0 memory.data }) 

    -- Increment value at pointer
    '+' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, { memory | data = Dict.insert memory.pointer (sanitizeInt program.numBits (update (+))) memory.data})

    -- Decrement value at pointer
    '-' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      }, { memory | data = Dict.insert memory.pointer (sanitizeInt program.numBits (update (-))) memory.data})

    -- Output value at pointer
    '.' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      , output = Char.fromCode currentData :: program.output}, memory)

    -- Input value into pointer
    ',' -> ({program 
      | instructionPointer = program.instructionPointer + 1
      , inputChars = Maybe.withDefault [] (List.tail program.inputChars)
      }, { memory | data = Dict.insert memory.pointer (sanitizeInt program.numBits (Char.toCode <| Maybe.withDefault '\u{0000}' (List.head program.inputChars))) memory.data})

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