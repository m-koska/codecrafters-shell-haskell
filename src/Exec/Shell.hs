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
import System.Environment (executablePath, lookupEnv)
import System.IO
import System.OsPath
import System.Posix
import System.Posix.Process
import System.Process

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Read as T.R
import qualified GHC.IO.Handle as IOHandle

import Exec.Command
import Parse.Parser
import Types
import Data.List
import Text.Printf (printf)
import Control.Applicative
import qualified Data.Text.IO as T
import Data.Maybe

processCommand :: AST -> Shell Bool 
processCommand (ExecNode command) = executeCommand command

processCommand (BackgroundJobNode inner_ast) = do
  modify $ \s -> s { is_next_cmd_in_bg = True }
  result <- processCommand inner_ast
  modify $ \s -> s { is_next_cmd_in_bg = False }
  return result

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

executeCommand (BuiltIn Exit) = do
  state <- get
  history_path <- liftIO $ lookupEnv "HISTFILE"
  
  case history_path of 
    Just path -> liftIO $ T.IO.writeFile path (T.unlines $ reverse (history state))
    
  return False

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
      case args_parsed of
        [path, cmd] -> do
          modify $ \s ->
            s
              { completions = Map.insert cmd (T.unpack path) (completions s)
              }

        _ ->
          liftIO $ T.IO.putStrLn "complete: invalid args"

    ("r":_) ->
      case args_parsed of
        [cmd] -> do
          modify $ \s ->
            s { completions = Map.delete cmd (completions s) }

        _ -> do
          liftIO $ T.IO.putStrLn "complete: invalid args"

    _ -> pure()

  return True

executeCommand (BuiltIn Jobs) = do
  state <- get
  let jobs_map = bg_jobs state

  jobs <- liftIO $ forM (Map.toList jobs_map) $ \(j_id, job_inf) -> do
    exit_code <- getProcessExitCode (job_handle job_inf) 
    return (j_id, job_inf, exit_code)

  let active_jobs = 
          [ (j_id, job)
          | (j_id, job, Nothing) <- jobs
          ]
  modify $ \s -> s { bg_jobs = Map.fromList active_jobs }

  let ids = sort [j_id | (j_id, _, _) <- jobs]  
  let (current, previous) = 
        case reverse ids of 
          (x:y:_) -> (Just x, Just y)
          [x]            -> (Just x, Nothing)
          _                    -> (Nothing, Nothing)

  forM_ jobs $ \(j_id, job, exit_code) -> do
      let sign
            | Just j_id == current  = "+"
            | Just j_id == previous = "-"
            | otherwise             = " " 

      let (status, job_cmd_str) = case exit_code of
            Just _ -> 
              ("Done" ++ replicate 17 ' ', job_cmd job)
            Nothing -> 
              ("Running" ++ replicate 14 ' ', T.concat[job_cmd job, " &"])

      let output = "[" ++ show j_id ++ "]" ++ sign ++ "  " ++ status ++ T.unpack (job_cmd job)
          
      liftIO $ putStrLn output

  return True

executeCommand (BuiltIn (History args)) = do
  state <- get
  
  let history_entries = 
          zip ([1..] :: [Int]) (reverse $ history state)
  
  let (flags, args_parsed) = parseFlags args

  if not (null flags)
    then 
      case flags of
        ("r":_) -> 
          case args_parsed of 
            [path] -> readHistory path
            _      -> liftIO $ putStrLn "history: invalid path"
          
        ("w":_) ->
          case args_parsed of
            [path] -> do
              let to_write = T.unlines $ reverse $ history state
              liftIO $ T.IO.writeFile (T.unpack path) to_write
              pure ()
            _      -> liftIO $ T.IO.putStrLn "history: invalid path"

        ("a":_) ->
          case args_parsed of
            [path] -> do
              let all_history = reverse $ history state
              let unwritten_history = drop (history_write_idx state) all_history
              let to_write = T.unlines unwritten_history

              liftIO $ T.IO.appendFile (T.unpack path) to_write
              modify $ \state -> state { history_write_idx = length all_history }

            _      -> liftIO $ T.IO.putStrLn "history: invalid path"

    else 
      case args_parsed of
        []     -> printHistory history_entries 
        [arg]  -> case T.R.decimal arg of
          Right (n, rest)
            -- let selected = take n $ reverse history_entries
            | T.null rest -> printHistory (reverse $ take n $ reverse history_entries)

          _ -> liftIO $ putStrLn "history: argument must be a number"

        _      -> liftIO $ putStrLn "history: invalid argument"

  return True
  where 
    printHistory :: [(Int, T.Text)] -> Shell ()
    printHistory hist = do
      forM_ hist $ \(i, cmd) -> do
        let line = T.pack (printf "%5d  " i) <> cmd
        liftIO $ T.IO.putStrLn line

    readHistory :: T.Text -> Shell ()
    readHistory file = do
      state <- get
      exists <- liftIO $ doesFileExist (T.unpack file)
      if exists
        then do
          contents <- T.lines <$> liftIO (T.IO.readFile $ T.unpack file)
          let new_history = reverse contents ++ history state 
          modify $ \state -> state { history = new_history } 
        else 
          liftIO $ T.IO.putStrLn "history: invalid path"


executeCommand (External command args) = do
  maybeCommandPath <- liftIO $ getCommand $ T.unpack command

  case maybeCommandPath of
    Nothing -> liftIO $ T.IO.putStrLn (T.concat[command, ": command not found"])
    
    Just _ -> do
      -- proc constructor
      let process = proc (T.unpack command) (map T.unpack args)

      (_, _, _, processHandle) <- liftIO $ createProcess process
      
      state <- get
      if is_next_cmd_in_bg state 
        then do
          pid <- liftIO $ getPid processHandle

          let os_pid = maybe "unknown" show pid
          let j_id = next_job_id state

          let args_str = T.concat[if null args then "" else " ", T.unwords args]
          let cmd_str = T.concat[command, args_str]
          let job_info = JobInfo processHandle cmd_str

          modify $ \s -> s
            { bg_jobs = Map.insert j_id job_info (bg_jobs s)
            , next_job_id = j_id + 1
            }

          liftIO $ putStrLn $ "[" ++ show j_id ++ "] " ++ os_pid 

        else do
          _ <- liftIO $ waitForProcess processHandle
          pure() 
  return True