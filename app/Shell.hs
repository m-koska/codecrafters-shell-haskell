module Shell where

import qualified Data.Text as T

import Command
import Parser 
import qualified Data.Text.IO as T.IO
import System.Posix.Process
import System.IO
import System.Directory
import System.OsPath
import System.Posix (changeWorkingDirectory)

executeCommand :: Command -> IO Bool

executeCommand Blank = return True

executeCommand (BuiltIn (Cd args)) = do

  exists <- doesDirectoryExist $ T.unpack args

  if exists
    then do
      home_directory <- getHomeDirectory
      let home_path = T.pack home_directory
      let path = T.replace (T.pack "~") home_path args
      changeWorkingDirectory $ T.unpack args
    else T.IO.putStrLn $ T.concat["cd: ", args, ": No such file or directory"]

  return True

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
        

executeCommand (BuiltIn Pwd) = do
  current_directory <- getCurrentDirectory
  putStrLn current_directory
  return True

executeCommand (External command args) = do
  
  maybeCommandPath <- getCommand $ T.unpack command

  case maybeCommandPath of
    Nothing -> T.IO.putStrLn (T.concat[command, ": command not found"])
    Just x  -> do
      pid <- forkProcess $ do --process id
        executeFile (T.unpack command) True (map T.unpack args) Nothing
      -- wait until the process ends
      _ <- getProcessStatus True False pid
      pure ()
  return True