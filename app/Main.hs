{-|
Module      : Main
Description : Entry point for the Haskell UNIX Shell.
Author      : Michał Kośka

This module configures the REPL environment and starts the main interactive loop 
for the shell application.
-}

module Main (main) where

import System.IO

import qualified Data.Text.IO as T.IO
import qualified Data.Map as Map

import CLI.InputHandler
import Types
import Control.Monad.State (evalStateT)
import System.Environment (lookupEnv, getEnv)
import qualified Data.Text as T

main :: IO ()
main = do

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  -- run the REPL
  T.IO.putStr "$ "

  history_path <- lookupEnv "HISTFILE"
  history <- maybe (pure []) getHistory history_path 
    

  let initial_state =
        ShellState
        { buffer              = ""
        , prev_key            = OtherKey
        , completions         = Map.empty
        , bg_jobs             = Map.empty
        , next_job_id         = 1
        , is_next_cmd_in_bg   = False
        , history             = history
        , history_index       = 0
        , history_write_idx   = 0
        }

  evalStateT mainLoop initial_state
  where 
    getHistory path = do
      text_lines <- T.IO.readFile path
      return $ reverse $ T.lines text_lines