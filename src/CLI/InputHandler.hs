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
      liftIO $ T.IO.putStr "$ "
      mainLoop

    buffer -> do 
      liftIO $ putChar '\n'
      let (input_tokenised, state) = tokeniseInput (T.pack buffer)
      -- walking the AST tree
      case walkAST input_tokenised of
        Left err -> do
          liftIO $ T.IO.putStr $ T.concat ["syntax error: ", err]
          liftIO $ T.IO.putStr "$ "
          modify $ \s ->
            s { buffer = ""
              , prev_key = OtherKey
              }

          mainLoop      
        Right ast -> do
          continue <- processCommand ast
          when continue $ do
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
  

handleRegularChar :: Char -> Shell ()
handleRegularChar ch = do
  liftIO $ putChar ch
  
  modify $ \state ->
    state 
    { buffer   = buffer state ++ [ch]
    , prev_key = OtherKey 
    }

  mainLoop