{-|
Module      : CLI.InputHandler
Description : Manages interactive REPL keystrokes.
Author      : Michał Kośka

Handles raw character inputs from the terminal, managing special keystrokes 
such as Enter, Backspace, and Tab for the interactive prompt.
-}

module CLI.InputHandler where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import qualified Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Map as Map

import CLI.CompletionHandler
import Exec.Command
import Exec.Shell
import Parse.Parser
import Parse.Tokeniser
import Types
import System.Process (getProcessExitCode)
import Data.List
import Data.List.NonEmpty (append)
import System.IO

mainLoop :: Shell () 
mainLoop = do
  ch <- liftIO getChar
  
  case ch of
    '\n'    -> handleEnter 
    '\DEL'  -> handleBackspace
    '\t'    -> do
      state <- get 
      let (input_tokenised, token_state) = tokeniseInput (T.pack $ buffer state)
      handleTab input_tokenised token_state 
    '\ESC'  -> do
      is_an_arrow <- liftIO $ hReady stdin
      when is_an_arrow $ do
        c2 <- liftIO getChar
        c3 <- liftIO getChar 
        case (c2, c3) of 
          ('[', 'A') -> handleUpArrow
    regular -> handleRegularChar regular


-- Tab Handling
handleTab :: [T.Text] -> TokenState -> Shell ()
handleTab input_tokenised token_state = do
  handleCompletion input_tokenised token_state
  mainLoop

-- Enter Handling
handleEnter:: Shell()
handleEnter = do
  state <- get

  case buffer state of
    "" -> do
      liftIO $ putChar '\n'
      reapJobs
      liftIO $ T.IO.putStr "$ "
      mainLoop

    buffer -> do 
      modify $ \s ->
        s { history = T.pack buffer : history s}
      liftIO $ putChar '\n'
      let (input_tokenised, state) = tokeniseInput (T.pack buffer)
      -- walking the AST tree
      case walkAST input_tokenised of
        Left err -> do
          liftIO $ T.IO.putStr $ T.concat ["syntax error: ", err]
          reapJobs
          liftIO $ T.IO.putStr "$ "
          modify $ \s ->
            s { buffer = ""
              , prev_key = OtherKey
              }

          mainLoop      
        Right ast -> do
          continue <- processCommand ast
          when continue $ do
            reapJobs
            liftIO $ T.IO.putStr "$ "
            modify $ \s ->
              s { buffer = ""
                , prev_key = OtherKey
                }

            mainLoop

-- Backspace Handling
handleBackspace :: Shell ()
handleBackspace = do

  state <- get

  case buffer state of
    ""  -> do
      liftIO $ putChar '\x07'

      modify $ \s ->
        s {prev_key = OtherKey}

    buf -> do
      liftIO $ T.IO.putStr "\b \b"

      modify $ \s ->
        s 
        { buffer   = Prelude.init buf
        , prev_key = OtherKey
        }

  mainLoop

handleUpArrow :: Shell ()
handleUpArrow = do
  state <- get 

  let current_history = history state
      current_index = history_index state
      max_index = length current_history
      
  let can_go = current_index < max_index
  if can_go
    then do
      let new_buffer = history state !! current_index
      modify $ \state -> 
        state { history_index = current_index + 1 }

      liftIO $ T.IO.putStr "\ESC[1K\r"
      liftIO $ T.IO.putStr $ "$ " <> new_buffer
    else
      liftIO $ putChar '\a'

  mainLoop

handleRegularChar :: Char -> Shell ()
handleRegularChar ch = do
  liftIO $ putChar ch
  
  modify $ \state ->
    state 
    { buffer   = buffer state ++ [ch]
    , prev_key = OtherKey 
    }

  mainLoop

reapJobs :: Shell () 
reapJobs = do 
  state <- get

  jobs <- liftIO $ forM (Map.toList (bg_jobs state)) $ \(j_id, job_inf) -> do
    exit_code <- getProcessExitCode (job_handle job_inf)
    return (j_id, job_inf, exit_code)

  let active_map = Map.fromList
        [ (j_id, job)
        | (j_id, job, Nothing) <- jobs
        ]

  let new_next_id =
        case Map.keys active_map of
          [] -> 1
          xs -> maximum xs + 1

  modify $ \s -> s
    { bg_jobs = active_map
    , next_job_id = new_next_id
    }

  let ids = sort [j_id | (j_id, _, _) <- jobs]

      (current, previous) =
        case reverse ids of
          (x:y:_) -> (Just x, Just y)
          [x]     -> (Just x, Nothing)
          _       -> (Nothing, Nothing)

  forM_ jobs $ \(j_id, job, exit_code) ->
    forM_ exit_code $ \_ -> do
      let sign
            | Just j_id == current  = "+"
            | Just j_id == previous = "-"
            | otherwise             = " "

          output =
            "[" ++ show j_id ++ "]"
            ++ sign
            ++ "  Done                 "
            ++ T.unpack (job_cmd job)

      liftIO $ putStrLn output