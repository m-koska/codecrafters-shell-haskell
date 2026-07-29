module Exec.BuiltIns
where

import Control.Monad 
import Control.Monad.IO.Class 
import Control.Monad.State 
import Data.Char
import Data.List
import System.Directory
import System.Environment 
import System.Process 
import Text.Printf 

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Read as T.R

import Exec.Command 
import Types
import System.Posix
import Parse.Parser

-- | Changes working directory with tilde (~) prefix expansion.
execCd :: T.Text -> Shell Bool
execCd args = do
  home_directory <- liftIO getHomeDirectory
  let home_path = T.pack home_directory
      path = expandTilde home_path args

  exists <- liftIO $ doesDirectoryExist (T.unpack path)
  if exists
    then liftIO $ changeWorkingDirectory (T.unpack path)
    else liftIO $ T.IO.putStrLn $ T.concat ["cd: ", path, ": No such file or directory"]

  return True
  where
    expandTilde home path
      | path == "~"              = home
      | "~/" `T.isPrefixOf` path = home <> T.drop 1 path
      | otherwise                = path

-- | Flushes history to HISTFILE before exiting.
execExit :: Shell Bool
execExit = do
  state <- get
  history_path <- liftIO $ lookupEnv "HISTFILE"
  case history_path of 
    Just path -> liftIO $ T.IO.writeFile path (T.unlines $ reverse (history state))
    Nothing   -> pure ()
  return False

-- | Prints arguments separated by space.
execEcho :: [T.Text] -> Shell Bool
execEcho args = do
  liftIO $ T.IO.putStrLn (T.unwords args)
  return True

-- | Resolves command type (builtin vs PATH executable).
execType :: [T.Text] -> Shell Bool
execType args = do
  mapM_ lookupCmd args
  return True
  where 
    lookupCmd command
      | isBuiltIn command = liftIO $ T.IO.putStrLn (T.concat [command, " is a shell builtin"])
      | otherwise = do 
        maybePath <- liftIO $ getCommand $ T.unpack command
        case maybePath of
          Nothing -> liftIO $ T.IO.putStrLn (T.concat [command, ": not found"]) 
          Just x  -> liftIO $ T.IO.putStrLn $ T.concat [command, " is ", T.pack x]

-- | Prints current working directory.
execPwd :: Shell Bool
execPwd = do
  current_directory <- liftIO getCurrentDirectory
  liftIO $ putStrLn current_directory
  return True

-- | Configures auto-completion specs in ShellState.
execComplete :: [T.Text] -> Shell Bool
execComplete args = do
  let (flags, args_parsed) = parseFlags args
  case flags of
    ("p":_) -> forM_ args_parsed $ \cmd -> do
      state <- get
      case Map.lookup cmd (completions state) of
        Nothing   -> liftIO $ T.IO.putStrLn (T.concat ["complete: ", cmd, ": no completion specification"])
        Just path -> liftIO $ T.IO.putStrLn $ T.concat ["complete -C '", T.pack path, "' ", cmd]

    ("C":_) -> case args_parsed of
      [path, cmd] -> modify $ \s -> s { completions = Map.insert cmd (T.unpack path) (completions s) }
      _           -> liftIO $ T.IO.putStrLn "complete: invalid args"

    ("r":_) -> case args_parsed of
      [cmd] -> modify $ \s -> s { completions = Map.delete cmd (completions s) }
      _     -> liftIO $ T.IO.putStrLn "complete: invalid args"

    _ -> pure ()
  return True

-- | Displays background jobs status table.
execJobs :: Shell Bool
execJobs = do
  state <- get
  let jobs_map = bg_jobs state

  jobs <- liftIO $ forM (Map.toList jobs_map) $ \(j_id, job_inf) -> do
    exit_code <- getProcessExitCode (job_handle job_inf) 
    return (j_id, job_inf, exit_code)

  let active_jobs = [(j_id, job) | (j_id, job, Nothing) <- jobs]
  modify $ \s -> s { bg_jobs = Map.fromList active_jobs }

  let ids = sort [j_id | (j_id, _, _) <- jobs]  
      (current, previous) = case reverse ids of 
        (x:y:_) -> (Just x, Just y)
        [x]     -> (Just x, Nothing)
        _       -> (Nothing, Nothing)

  forM_ jobs $ \(j_id, job, exit_code) -> do
    let sign
          | Just j_id == current  = "+"
          | Just j_id == previous = "-"
          | otherwise             = " " 

        (status, _) = case exit_code of
          Just _  -> ("Done" ++ replicate 17 ' ', job_cmd job)
          Nothing -> ("Running" ++ replicate 14 ' ', T.concat [job_cmd job, " &"])

        output = "[" ++ show j_id ++ "]" ++ sign ++ "  " ++ status ++ T.unpack (job_cmd job)
    liftIO $ putStrLn output

  return True

-- | Displays or persists command history.
execHistory :: [T.Text] -> Shell Bool
execHistory args = do
  state <- get
  let history_entries = zip ([1..] :: [Int]) (reverse $ history state)
      (flags, args_parsed) = parseFlags args

  if not (null flags)
    then case flags of
      ("r":_) -> case args_parsed of 
        [path] -> readHistory path
        _      -> liftIO $ putStrLn "history: invalid path"
        
      ("w":_) -> case args_parsed of
        [path] -> liftIO $ T.IO.writeFile (T.unpack path) (T.unlines $ reverse $ history state)
        _      -> liftIO $ T.IO.putStrLn "history: invalid path"

      ("a":_) -> case args_parsed of
        [path] -> do
          let all_history = reverse $ history state
              unwritten   = drop (history_write_idx state) all_history
          liftIO $ T.IO.appendFile (T.unpack path) (T.unlines unwritten)
          modify $ \s -> s { history_write_idx = length all_history }
        _      -> liftIO $ T.IO.putStrLn "history: invalid path"

      _ -> liftIO $ putStrLn "history: invalid flag"
    else case args_parsed of
      []    -> printHistory history_entries 
      [arg] -> case T.R.decimal arg of
        Right (n, rest) | T.null rest -> printHistory (reverse $ take n $ reverse history_entries)
        _                             -> liftIO $ putStrLn "history: argument must be a number"
      _     -> liftIO $ putStrLn "history: invalid argument"

  return True
  where 
    printHistory hist = forM_ hist $ \(i, cmd) -> do
      let line = T.pack (printf "%5d  " i) <> cmd
      liftIO $ T.IO.putStrLn line

    readHistory :: T.Text -> Shell ()
    readHistory file = do
      state <- get
      exists <- liftIO $ doesFileExist (T.unpack file)
      if exists
        then do
          contents <- T.lines <$> liftIO (T.IO.readFile $ T.unpack file)
          modify $ \s -> s { history = reverse contents ++ history s }
        else liftIO $ T.IO.putStrLn "history: invalid path"

-- | Manages shell variables declaration and validation.
execDeclare :: [T.Text] -> Shell Bool
execDeclare args = do
  state <- get
  let (flags, args_parsed) = parseFlags args

  case flags of 
    ("p":_) -> case args_parsed of
      [var] -> case Map.lookup var (shell_vars state) of
        Just val -> liftIO $ T.IO.putStrLn $ "declare -- " <> var <> "=\"" <> val <> "\""
        Nothing  -> liftIO $ T.IO.putStrLn $ "declare: " <> var <> ": not found"
      _       -> liftIO $ T.IO.putStrLn "declare: invalid variable provided"

    _ -> case args_parsed of
      [txt] -> case T.break (== '=') txt of
        (l, r)
          | T.null r  -> liftIO $ T.IO.putStrLn "declare: missing '='"
          | otherwise -> do
              let (var, val) = (l, T.drop 1 r)
              if validateIdentifier var
                then modify $ \s -> s { shell_vars = Map.insert var val (shell_vars s) }
                else liftIO $ T.IO.putStrLn $ "declare: `" <> var <> "=" <> val <> "': not a valid identifier"
      _     -> pure ()

  return True
  where 
    validateIdentifier var_name = case T.uncons var_name of
      Just (ch, rest) -> (isLetter ch || ch == '_') && T.all (\c -> isLetter c || isNumber c || c == '_') rest
      Nothing         -> False