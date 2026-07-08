{-|
Module      : CLI.InputHandler
Description : Manages interactive REPL keystrokes.
Author      : Michał Kośka

Handles raw character inputs from the terminal, managing special keystrokes 
such as Enter, Backspace, and Tab for the interactive prompt.
-}

module CLI.InputHandler where

import Control.Monad
import qualified Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import CLI.CompletionHandler
import Exec.Command
import Exec.Shell
import Parse.Parser
import Parse.Tokeniser
import Types

data ShellState = ShellState 
  { buffer   :: String
  , prev_key :: KeyType 
  }

mainLoop:: ShellState -> IO () 
mainLoop state = do
  ch <- getChar
  case ch of
    '\n'    -> handleEnter state 
    '\DEL'  -> handleBackspace state
    '\t'     -> handleTab state
    regular -> handleRegularChar state regular

-- Tab Handling
handleTab :: ShellState -> IO()
handleTab state = do
  (new_buffer, new_key) <- handleCompletion (buffer state) (prev_key state)
  mainLoop state {buffer = new_buffer, prev_key = new_key}

-- Enter Handling
handleEnter:: ShellState -> IO()
handleEnter ShellState {buffer = ""} = do
  putChar '\n'
  T.IO.putStr "$ "
  mainLoop ShellState {buffer = "", prev_key = OtherKey}

handleEnter ShellState {buffer = buffer, prev_key = prev_key} = do
  putChar '\n'
  let input_tokenised = tokeniseInput (T.pack buffer)
  -- walking the AST tree
  case walkAST input_tokenised of

    Left err -> do
      T.IO.putStr $ T.concat ["syntax error: ", err]
      T.IO.putStr "$ "
      mainLoop ShellState {buffer = "", prev_key = OtherKey}
  
    Right ast -> do
      continue <- processCommand ast
      when continue $ do
        T.IO.putStr "$ "
        mainLoop ShellState {buffer = "", prev_key = OtherKey}

-- Backspace Handling
handleBackspace :: ShellState -> IO ()
handleBackspace ShellState {buffer = ""} = do
  putChar '\x07'
  mainLoop ShellState {buffer = "", prev_key = OtherKey}

handleBackspace state =
  case buffer state of
    ""  -> do
      putChar '\x07'
      mainLoop ShellState {buffer = "", prev_key = OtherKey}
    buf -> do 
      T.IO.putStr "\b \b"
      mainLoop ShellState {buffer = Prelude.init buf, prev_key = OtherKey}
  

handleRegularChar :: ShellState -> Char -> IO ()
handleRegularChar ShellState {buffer = buffer} ch = do
  putChar ch
  mainLoop ShellState {buffer = buffer ++ [ch], prev_key = OtherKey}