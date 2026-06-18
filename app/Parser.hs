module Parser where

import Command
import qualified Data.Text as T

data AST 
  = ExecNode Command
  | RedirectNode AST FilePath 

parseRedirection :: [T.Text] -> Either T.Text AST
parseRedirection token_list = 
  let (left_side, right_side) = break (\x -> x `elem` [">", "1>"]) token_list
  
  in case right_side of
    
    [] -> parseCommand left_side

    (operator : file : rest) -> 
      case parseRedirection (left_side ++ rest) of 
        Left _err     -> Left _err
        Right cmd_ast -> Right (RedirectNode cmd_ast (T.unpack file))
    
    [_] -> Left "error"

parseCommand :: [T.Text] -> Either T.Text AST
parseCommand input_command = case input_command of
  ["cd"]       -> Right $ ExecNode (BuiltIn (Cd "~"))
  ("cd":(x:args)) -> Right $ ExecNode (BuiltIn (Cd x))
  ("exit":_)      -> Right $ ExecNode (BuiltIn Exit)
  ("echo":args)   -> Right $ ExecNode (BuiltIn (Echo args))
  ("type":args)   -> Right $ ExecNode (BuiltIn (Type args))
  ("pwd":_)       -> Right $ ExecNode (BuiltIn Pwd)
  (unknown:args)  -> Right $ ExecNode (External unknown args)
  _               -> Right $ ExecNode Blank