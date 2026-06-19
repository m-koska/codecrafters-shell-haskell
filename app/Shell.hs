module Shell where

import qualified Data.Text as T

import Command
import Parser 
import qualified Data.Text.IO as T.IO
import System.Posix.Process
import System.IO
import System.Directory
import System.OsPath
import System.Posix
import qualified GHC.IO.Handle as T
import System.Posix (dupTo)
import Control.Exception (bracket)
import System.Environment (executablePath)
import System.Process

processCommand :: AST -> IO Bool 

processCommand (ExecNode command) = executeCommand command

processCommand (RedirectNode redirection_type deeper_ast file write_method) = do
  
  T.hFlush stdout
  T.hFlush stderr
  -- bracket: try something, do another thing afterwards in case it fails
  -- bracket setup teardown try_something

  let

    target_std_fd = case redirection_type of
      StandardRedirection -> stdOutput
      ErrorRedirection    -> stdError

    posix_flags = case write_method of
        TruncateMethod -> defaultFileFlags { trunc = True, creat = Just 0o644 }
        AppendMethod   -> defaultFileFlags { append = True, creat = Just 0o644 }
  
    setup :: IO Fd
    setup = do

      backup_stdout <- dup target_std_fd
      write_fd <- openFd file WriteOnly posix_flags

      dupTo write_fd target_std_fd
      closeFd write_fd

      return backup_stdout

    teardown :: Fd -> IO ()
    teardown backup_stdout = do
      T.hFlush stdout
      dupTo backup_stdout target_std_fd
      closeFd backup_stdout

  bracket setup teardown (\_ -> processCommand deeper_ast)


-- builtin commands handling
executeCommand :: Command -> IO Bool

executeCommand Blank = return True

executeCommand (BuiltIn (Cd args)) = do

  home_directory <- getHomeDirectory
  let home_path = T.pack home_directory
  let path = T.replace (T.pack "~") home_path args
  
  exists <- doesDirectoryExist $ T.unpack path
  if exists
    then do
      changeWorkingDirectory $ T.unpack path
    else T.IO.putStrLn $ T.concat["cd: ", path, ": No such file or directory"]

  return True

executeCommand (BuiltIn Exit) = return False

executeCommand (BuiltIn (Echo args))  = do
  T.IO.putStrLn $ T.unwords args
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
    
    Just _ -> do
      -- proc constructort
      let process = proc (T.unpack command) (map T.unpack args)

      (_, _, _, processHandle) <- createProcess process
      _ <- waitForProcess processHandle

      pure ()
  
  return True