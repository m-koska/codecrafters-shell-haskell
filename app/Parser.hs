{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Command
import qualified Data.Text as T

data ArgsState = NormalText
  | SingleQuoteText

-- Zwraca stringa funkcji i stringa jej argumentów
-- albo nic jak jest źle wpisane 
parseInput :: String -> [T.Text]
parseInput input = 
  let 
    tokens_raw = go NormalText "" input
    tokens_filtered = filter (not . null) tokens_raw
    tokens_right_order = map reverse tokens_filtered 

  in map T.pack tokens_right_order
  
  where 
    -- ArgsState  - qouting etc
    -- String     - current accumulated argument
    -- String     - remaining text
    go :: ArgsState -> String -> String -> [String]
    -- 1. the current character is a "" (nothing) or a space
    go _ acc [] = [acc]
    -- add finished word to the end of the list
    go NormalText acc (' ':xs) = acc : go NormalText "" xs

    -- 2. special caracters - state change
    go NormalText acc ('\'':xs) = go SingleQuoteText acc xs
    
    -- 3. SingleQuoteText
    go SingleQuoteText acc ('\'':xs) = go NormalText acc xs
    go SingleQuoteText acc (x:xs) = go SingleQuoteText (x:acc) xs

    -- default go (take 'x' and glue it in front of 'acc'):
    go NormalText acc (x:xs) = go NormalText (x:acc) xs 

parseCommand :: [T.Text] -> Command
parseCommand input_command = case input_command of
  ("cd":(x:args))     -> BuiltIn (Cd x) 
  ("exit":_)      -> BuiltIn Exit
  ("echo":args)   -> BuiltIn (Echo args)
  ("type":args)   -> BuiltIn (Type args)
  ("pwd":_)       -> BuiltIn Pwd
  (unknown:args)  -> External unknown args
  _               -> Blank