module Shell where

import Command
import Parser 

executeCommand :: Command -> IO Bool

executeCommand Blank = return True

executeCommand (BuiltIn Exit) = return False

executeCommand (BuiltIn (Echo args))  = do
  putStrLn args
  return True

executeCommand (BuiltIn (Type args)) = do
  mapM_ lookup args
  return True
  where 
  lookup :: String -> IO ()
  lookup command
    | isBuiltIn command = putStrLn (command  ++ " is a shell builtin")
    | otherwise = putStrLn (command ++ ": command not found")
    
executeCommand (External command _) = do
  putStrLn (command ++ ": command not found")
  return True
