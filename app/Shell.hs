module Shell where

import qualified Data.Text as T

import Command
import Parser 
import qualified Data.Text.IO as T.IO

executeCommand :: Command -> IO Bool

executeCommand Blank = return True

executeCommand (BuiltIn Exit) = return False

executeCommand (BuiltIn (Echo args))  = do
  print args
  return True

executeCommand (BuiltIn (Type args)) = do
  mapM_ lookup args
  return True
  where 
  lookup :: T.Text -> IO ()
  lookup command
    | isBuiltIn command = T.IO.putStrLn (T.concat[command, " is a shell builtin"])
    | otherwise = T.IO.putStrLn (T.concat[command, ": not found"])
    
executeCommand (External command _) = do
  T.IO.putStrLn (T.concat[command, ": command not found"])
  return True
