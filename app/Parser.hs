{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Command
import qualified Data.Text as T

-- Zwraca stringa funkcji i stringa jej argumentów
-- albo nic jak jest źle wpisane 
parseInput :: T.Text -> Maybe (T.Text, T.Text)
parseInput input = case T.words input of
  []       -> Nothing
  (c:args) -> Just (c, T.unwords args)

parseCommand :: Maybe (T.Text, T.Text) -> Command
parseCommand input_command = case input_command of
  Nothing     -> Blank
  Just ("exit", _)     -> BuiltIn Exit
  Just ("echo", args)  -> BuiltIn (Echo args)
  Just ("type", args)  -> BuiltIn (Type (T.words args))
  Just (unknown_command, args) -> External unknown_command args