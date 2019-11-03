module Interpreter exposing (..)

import Array
import Dict exposing (Dict)

type alias Memory = {
  pointer : Int
  , data : List Int
  }

defaultMemory : Memory
defaultMemory = {
  pointer = 0
  , data = []
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
    , brackets: Dict Int Int
  }

type alias Output = Maybe Char

interpret : Program -> Memory -> (Memory, Output)
interpret prog memory = (defaultMemory, Nothing)

--String's toList function returns a List Char (may be useful!)

interpretInstruction : Char -> Memory -> (Memory, Output)
interpretInstruction instruction memory = 
  let
    modify : (Int -> Int -> Int) -> (Int -> Int -> Int)
    modify operation = (\index -> \cVal -> if index == memory.pointer then operation cVal 1 else cVal)
  in
  
  case instruction of
    -- Increment pointer
    '>' -> ({ memory | pointer = memory.pointer + 1}, Nothing)

    -- Decrement pointer
    '<' -> ({ memory | pointer = memory.pointer - 1}, Nothing)

    -- Increment value at pointer
    '+' -> ({ memory | data = List.indexedMap (modify (+)) memory.data}, Nothing)

    -- Decrement value at pointer
    '-' -> ({ memory | data = List.indexedMap (modify (-)) memory.data}, Nothing)

    -- Output value at pointer
    '.' -> (memory, Just (Char.fromCode (Maybe.withDefault 0 (Array.get memory.pointer (Array.fromList memory.data)))))

    -- Input value into pointer (currently not implemented)
    ',' -> (memory, Nothing)

    -- Begin loop
    '[' -> (memory, Nothing)

    ']' -> (memory, Nothing)

    _ -> (defaultMemory, Nothing) 