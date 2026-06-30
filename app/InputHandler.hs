module InputHandler where

import qualified Data.Text as T
import qualified Data.List

import Command
import Parser
import Shell
import Tokeniser
import qualified Data.Text.IO as T.IO
import Control.Monad


mainLoop:: String -> IO () 
mainLoop buffer = do
  ch <- getChar
  case ch of
    '\n'    -> handleEnter buffer 
    '\DEL'  -> handleBackspace buffer
    '\t'     -> handleTab buffer
    regular -> handleRegularChar buffer regular

-- Tab Handling

handleTab :: String -> IO()
handleTab input = do
  let input_text = T.pack input
      matching_builtins = filter (T.isPrefixOf input_text) builtins
  
  matching_ext <- getMatchingExecutables input_text
  
  let matches = Data.List.nub (matching_builtins ++ matching_ext)   
  
  case matches of
    
    [only_one] -> do
      let current_length = length input
          to_put         = T.drop current_length only_one    
      T.IO.putStr $ T.concat [to_put, " "] 
      mainLoop $ T.unpack only_one ++ " "

    _          -> do
      putChar '\x07'
      mainLoop input


-- Enter Handling
handleEnter:: String -> IO()
handleEnter "" = do
  putChar '\n'
  T.IO.putStr "$ "

  mainLoop ""

handleEnter buffer = do
  putChar '\n'
  
  let input_tokenised = tokeniseInput (T.pack buffer)
  -- walking the AST tree
  case walkAST input_tokenised of

    Left err -> do
      T.IO.putStr $ T.concat ["syntax error: ", err]
      T.IO.putStr "$ "
      mainLoop ""
  
    Right ast -> do
      continue <- processCommand ast
      when continue $ do
        T.IO.putStr "$ "
        mainLoop ""

-- Backspace Handling

handleBackspace :: String -> IO ()
handleBackspace "" = do mainLoop ""

handleBackspace input = do
  T.IO.putStr "\b \b"
  mainLoop (Prelude.init input)

handleRegularChar :: String -> Char -> IO ()
handleRegularChar buffer ch = do

  putChar ch
  mainLoop (buffer ++ [ch])