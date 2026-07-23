{-|
Module      : Exec.Shell
Description : Evaluates and executes the parsed AST.
Author      : Michał Kośka

Walks the Abstract Syntax Tree, managing POSIX file descriptors for redirections, 
and dispatches execution to either built-in shell functions or external binaries.
-}

module Exec.Shell where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS
import System.Directory
import System.Environment (executablePath)
import System.IO
import System.OsPath
import System.Posix
import System.Posix.Process
import System.Process

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified GHC.IO.Handle as IOHandle

import Exec.Command
import Parse.Parser
import Types

processCommand :: AST -> Shell Bool 

processCommand (ExecNode command) = executeCommand command

processCommand (RedirectNode redirection_type deeper_ast file write_method) = do
  
  liftIO $ IOHandle.hFlush stdout
  liftIO $ IOHandle.hFlush stderr
  -- bracket: try something, do another thing afterwards in case it fails
  -- bracket setup teardown try_something

  let

    target_std_fd = case redirection_type of
      StandardRedirection -> stdOutput
      ErrorRedirection    -> stdError

    posix_flags = case write_method of
        TruncateMethod -> defaultFileFlags { trunc = True, creat = Just 0o644 }
        AppendMethod   -> defaultFileFlags { append = True, creat = Just 0o644 }
  
    setup :: Shell Fd
    setup = do

      backup_stdout <- liftIO $ dup target_std_fd
      write_fd <- liftIO $ openFd file WriteOnly posix_flags

      liftIO $ dupTo write_fd target_std_fd
      liftIO $ closeFd write_fd

      return backup_stdout

    teardown :: Fd -> Shell ()
    teardown backup_stdout = do
      liftIO $ IOHandle.hFlush stdout
      liftIO $ dupTo backup_stdout target_std_fd
      liftIO $ closeFd backup_stdout

  bracket setup teardown (\_ -> processCommand deeper_ast)


-- builtin commands handling
executeCommand :: Command -> Shell Bool

executeCommand Blank = return True

executeCommand (BuiltIn (Cd args)) = do

  home_directory <- liftIO getHomeDirectory
  let home_path = T.pack home_directory
  let path = T.replace (T.pack "~") home_path args
  
  exists <- liftIO $ doesDirectoryExist (T.unpack path)
  if exists
    then do
      liftIO $ changeWorkingDirectory (T.unpack path)
    else liftIO $ T.IO.putStrLn $ T.concat["cd: ", path, ": No such file or directory"]

  return True

executeCommand (BuiltIn Exit) = return False

executeCommand (BuiltIn (Echo args))  = do
  liftIO $ T.IO.putStrLn (T.unwords args)
  return True

executeCommand (BuiltIn (Type args)) = do
  mapM_ lookup args
  return True
  where 
  lookup :: T.Text -> Shell ()
  lookup command
    | isBuiltIn command = liftIO $ T.IO.putStrLn (T.concat[command, " is a shell builtin"])
    | otherwise = do 
      maybeCommandPath <- liftIO $ getCommand $ T.unpack command
      case maybeCommandPath of
        Nothing -> liftIO $ T.IO.putStrLn (T.concat[command, ": not found"]) 
        Just x  -> liftIO $ T.IO.putStrLn $ T.concat[command, " is ", T.pack x]

executeCommand (BuiltIn Pwd) = do
  current_directory <- liftIO getCurrentDirectory
  liftIO $ putStrLn current_directory
  return True

executeCommand (BuiltIn (Complete args)) = do
  let (flags, args_parsed) = parseFlags args

  case flags of
    ("p":_) -> mapM_ printComplete args_parsed 
      where 
        printComplete :: T.Text -> Shell ()
        printComplete cmd = do
              state <- get
              case Map.lookup cmd (completions state) of
                Nothing   -> liftIO $ T.IO.putStrLn (T.concat["complete: ", cmd, ": no completion specification"])
                Just path ->
                  liftIO $ T.IO.putStrLn $
                    T.concat ["complete -C '", T.pack path, "' ", cmd]         

    ("C":_) ->
      if length args_parsed == 2
        then do 
          let path = head args_parsed
              cmd  = args_parsed !! 1
          modify $ \s ->
            s 
            {
              completions = Map.insert cmd (T.unpack path) (completions s)
            }
        else do liftIO $ T.IO.putStrLn "complate: invalid args"

    ("r":_) ->
      if length args_parsed == 1
        then do
          let cmd = head args_parsed
          modify $ \s ->
            s
            {
              completions = Map.delete cmd (completions s)
            }
        else do liftIO $ T.IO.putStrLn "complate: invalid args"

    _ -> pure()

  return True

executeCommand (BuiltIn Jobs) = do
  liftIO $ putChar '\n'
  return True

executeCommand (External command args) = do

  maybeCommandPath <- liftIO $ getCommand $ T.unpack command

  case maybeCommandPath of
    
    Nothing -> liftIO $ T.IO.putStrLn (T.concat[command, ": command not found"])
    
    Just _ -> do
      -- proc constructort
      let process = proc (T.unpack command) (map T.unpack args)

      (_, _, _, processHandle) <- liftIO $ createProcess process
      _ <- liftIO $ waitForProcess processHandle

      pure ()
  
  return True