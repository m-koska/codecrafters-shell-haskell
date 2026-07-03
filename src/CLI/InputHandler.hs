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

mainLoop:: String -> KeyType -> IO () 
mainLoop buffer prev_key = do
  ch <- getChar
  case ch of
    '\n'    -> handleEnter buffer 
    '\DEL'  -> handleBackspace buffer
    '\t'     -> handleTab buffer prev_key
    regular -> handleRegularChar buffer regular

-- Tab Handling
handleTab :: String -> KeyType -> IO()
handleTab input prev_key = do
  (new_buffer, new_key) <- handleCompletion input prev_key
  mainLoop new_buffer new_key

-- Enter Handling
handleEnter:: String -> IO()
handleEnter "" = do
  putChar '\n'
  T.IO.putStr "$ "
  mainLoop "" OtherKey

handleEnter buffer = do
  putChar '\n'
  let input_tokenised = tokeniseInput (T.pack buffer)
  -- walking the AST tree
  case walkAST input_tokenised of

    Left err -> do
      T.IO.putStr $ T.concat ["syntax error: ", err]
      T.IO.putStr "$ "
      mainLoop "" OtherKey
  
    Right ast -> do
      continue <- processCommand ast
      when continue $ do
        T.IO.putStr "$ "
        mainLoop "" OtherKey

-- Backspace Handling
handleBackspace :: String -> IO ()
handleBackspace "" = do mainLoop "" OtherKey

handleBackspace input = do
  T.IO.putStr "\b \b"
  mainLoop (Prelude.init input) OtherKey

handleRegularChar :: String -> Char -> IO ()
handleRegularChar buffer ch = do

  putChar ch
  mainLoop (buffer ++ [ch]) OtherKey