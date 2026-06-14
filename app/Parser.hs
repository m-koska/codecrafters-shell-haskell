module Parser where

import Command

-- Zwraca stringa funkcji i stringa jej argumentów
-- albo nic jak jest źle wpisane 
parseInput :: String -> Maybe (String, String)
parseInput input = case words input of
  []       -> Nothing
  (c:args) -> Just (c, unwords args)

parseCommand :: Maybe (String, String) -> Command
parseCommand input_command = case input_command of
  Nothing     -> Blank
  Just ("exit", _)     -> BuiltIn Exit
  Just ("echo", args)  -> BuiltIn (Echo args)
  Just ("type", args)  -> BuiltIn (Type (words args))
  Just (unknown_command, args) -> External unknown_command args