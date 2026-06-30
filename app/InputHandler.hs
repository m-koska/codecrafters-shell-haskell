module InputHandler where

import qualified Data.Text as T
import qualified Data.List

import Command
import Parser
import Shell
import Tokeniser
import qualified Data.Text.IO as T.IO
import Control.Monad
import qualified Data.Text.IO as T


data KeyType
  = TabKey
  | OtherKey 

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
  let input_text = T.pack input
      matching_builtins = filter (T.isPrefixOf input_text) builtins
  
  matching_ext <- getMatchingExecutables input_text
  
  let matches = Data.List.sort $ Data.List.nub (matching_builtins ++ matching_ext)   
  
  case (matches, prev_key) of
    ([], _) -> do
          putChar '\x07'
          mainLoop input OtherKey    
    ([only_one], _) -> do
      let current_length = length input
          to_put         = T.drop current_length only_one    
      T.IO.putStr $ T.concat [to_put, " "] 
      mainLoop (T.unpack only_one ++ " ") OtherKey

    (_, _) -> do
      let matches_str = map T.unpack matches
          lcp = longestCommonPrefix matches_str
      
      if length lcp > length input
        then do
          let to_put = drop (length input) lcp
          T.IO.putStr $ T.pack to_put
          mainLoop lcp OtherKey

        else do
          
          case prev_key of
            TabKey -> do
              putChar '\n'
              T.IO.putStr $ T.intercalate "  " matches
              putChar '\n'
              T.IO.putStr $ T.concat ["$ ", input_text]
              mainLoop input TabKey
            OtherKey -> do
              putChar '\x07'
              mainLoop input TabKey


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

-- simple longest commpn prefix algorithm, for tab completions  
commonPrefix :: String -> String -> String
commonPrefix (x:xs) (y:ys) 
  | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = ""


longestCommonPrefix :: [String] -> String
longestCommonPrefix = foldl1 commonPrefix