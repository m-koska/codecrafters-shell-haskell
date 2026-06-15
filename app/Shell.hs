module Shell where

import qualified Data.Text as T

import Command
import Parser 
import qualified Data.Text.IO as T.IO
import System.Posix.Process

executeCommand :: Command -> IO Bool

executeCommand Blank = return True

executeCommand (BuiltIn Exit) = return False

executeCommand (BuiltIn (Echo args))  = do
  T.IO.putStrLn args
  return True

executeCommand (BuiltIn (Type args)) = do
  mapM_ lookup args
  return True
  where 
  lookup :: T.Text -> IO ()
  lookup command
    | isBuiltIn command = T.IO.putStrLn (T.concat[command, " is a shell builtin"])
    | otherwise = do 
        maybeCommandPath <- getCommand $ T.unpack command
        case maybeCommandPath of
          Nothing -> T.IO.putStrLn (T.concat[command, ": not found"]) 
          Just x  -> T.IO.putStrLn $ T.concat[command, " is ", T.pack x]
        
    
executeCommand (External command args) = do
  
  maybeCommandPath <- getCommand $ T.unpack command

  case maybeCommandPath of
    Nothing -> T.IO.putStrLn (T.concat[command, ": command not found"])
    Just x  -> executeFile (T.unpack command) True (map T.unpack args) Nothing
    -- dzięki temu, ze jest True, samo sobie wyszuka w PATH
  return True