module Interpreter exposing (..)

import Array

type alias Memory = {
  pointer : Int
  , data : List Int
  }

defaultMemory : Memory
defaultMemory = {
  pointer = 0
  , data = []
  }


type alias Program = String
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
    '[' -> ({ memory | 
      pointer = --TODO
      }, Nothing)

    _ -> (defaultMemory, Nothing) 