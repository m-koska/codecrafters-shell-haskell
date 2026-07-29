{-|
Module      : Exec.Shell
Description : Evaluates and executes the parsed AST.
Author      : Michał Kośka

Walks the Abstract Syntax Tree, managing POSIX file descriptors for redirections, 
and dispatches execution to either built-in shell functions or external binaries.
-}

module Exec.Shell 
  ( processCommand
  , executeCommand
  ) where

import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, evalStateT)
import System.Posix (Fd, openFd, closeFd, dup, dupTo, stdOutput, stdError, OpenMode(WriteOnly), defaultFileFlags, trunc, append, creat, forkProcess, createPipe, exitImmediately, stdInput, getProcessStatus)
import System.Process (proc, createProcess, waitForProcess, getPid)
import System.Exit
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified GHC.IO.Handle as IOHandle
import System.IO (stdout, stderr)

import Exec.Command (getCommand)
import Types
import Exec.BuiltIns

-- | Evaluates AST nodes (ExecNode, BackgroundJobNode, RedirectNode).
processCommand :: AST -> Shell Bool 
processCommand (ExecNode command) = executeCommand command

processCommand (BackgroundJobNode inner_ast) = do
  modify $ \s -> s { is_next_cmd_in_bg = True }
  result <- processCommand inner_ast
  modify $ \s -> s { is_next_cmd_in_bg = False }
  return result

processCommand (PipeNode left_ast right_ast) = do
  state <- get

  (read_fd, write_fd) <- liftIO createPipe

  pid_left <- liftIO $ forkProcess $ do
    _ <- dupTo write_fd stdOutput 
    closeFd read_fd 
    closeFd write_fd 
    _ <- evalStateT (processCommand left_ast) state 
    exitImmediately ExitSuccess

  pid_right <- liftIO $ forkProcess $ do
    _ <- dupTo read_fd stdInput
    closeFd write_fd 
    closeFd read_fd
    _ <- evalStateT (processCommand right_ast) state
    exitImmediately ExitSuccess

  liftIO $ closeFd read_fd
  liftIO $ closeFd write_fd
  
  _ <- liftIO $ getProcessStatus True False pid_left
  _ <- liftIO $ getProcessStatus True False pid_right
  
  return True
  
-- | Command execution dispatcher.
executeCommand :: Command -> Shell Bool
executeCommand Blank                     = return True
executeCommand (BuiltIn (Cd args))       = execCd args
executeCommand (BuiltIn Exit)            = execExit
executeCommand (BuiltIn (Echo args))     = execEcho args
executeCommand (BuiltIn (Type args))     = execType args
executeCommand (BuiltIn Pwd)             = execPwd
executeCommand (BuiltIn (Complete args)) = execComplete args
executeCommand (BuiltIn Jobs)            = execJobs
executeCommand (BuiltIn (History args))  = execHistory args
executeCommand (BuiltIn (Declare args))  = execDeclare args
executeCommand (External command args)   = execExternal command args

-- | Spawns external process execution.
execExternal :: T.Text -> [T.Text] -> Shell Bool
execExternal command args = do
  maybePath <- liftIO $ getCommand $ T.unpack command
  case maybePath of
    Nothing -> liftIO $ T.IO.putStrLn (T.concat [command, ": command not found"])
    Just _  -> do
      let process = proc (T.unpack command) (map T.unpack args)
      (_, _, _, processHandle) <- liftIO $ createProcess process
      state <- get
      if is_next_cmd_in_bg state 
        then do
          pid <- liftIO $ getPid processHandle
          let os_pid   = maybe "unknown" show pid
              j_id     = next_job_id state
              args_str = T.concat [if null args then "" else " ", T.unwords args]
              cmd_str  = T.concat [command, args_str]
              job_info = JobInfo processHandle cmd_str

          modify $ \s -> s
            { bg_jobs     = Map.insert j_id job_info (bg_jobs s)
            , next_job_id = j_id + 1
            }
          liftIO $ putStrLn $ "[" ++ show j_id ++ "] " ++ os_pid 
        else do
          _ <- liftIO $ waitForProcess processHandle
          pure ()
  return True