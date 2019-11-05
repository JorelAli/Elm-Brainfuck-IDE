module Interpreter exposing (..)

-- import Array
import Dict exposing (Dict)

type alias Memory = {
  pointer : Int
  , data : Dict Int Int
  }

defaultMemory : Memory
defaultMemory = {
  pointer = 0
  , data = Dict.empty
  }

type ProgramState = Running | AwaitingInput

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
    , state : ProgramState
  }

createProgram : String -> Program 
createProgram input = {
    program = input
    , jumpMap = generateJumpMap input
    , instructionPointer = 0
    , output = []
    , state = Running
  }

generateJumpMap : String -> Dict Int Int
generateJumpMap program = 
  let
    doubleDict : Dict Int Int -> Dict Int Int
    doubleDict dict = List.foldl 
      (\elem -> \acc -> 
        case elem of 
          (k, v) -> Dict.insert v k acc) dict (Dict.toList dict)

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

printOutput : (Program, Memory) -> String
printOutput result = 
  Tuple.first result 
  |> .output 
  |> String.fromList 
  |> String.reverse

countBrackets : String -> (Int, Int)
countBrackets str = 
  String.foldl (\c -> \(x, y) ->
    case c of 
      '[' -> (x + 1, y)
      ']' -> (x, y + 1)
      _ -> (x, y)) (0, 0) str



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
      , state = AwaitingInput
      }, memory)

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