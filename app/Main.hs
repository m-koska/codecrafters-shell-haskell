module Main (main) where

import System.IO (hFlush, stdout)
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Command
import Parser
import Tokeniser
import Shell
import System.OsPath
import System.Posix (homeDirectory)

main :: IO ()
main = do
  mainLoop True 
  return ()

mainLoop :: Bool -> IO ()

mainLoop False = pure ()

mainLoop True = do 
  
  putStr "$ "
  hFlush stdout
  
  input_raw <- T.IO.getLine
  let input_tokenised = tokeniseInput input_raw
      
  case parseRedirection input_tokenised of
    -- error handling
    Left err -> do
      T.IO.putStrLn $ T.concat ["syntax error: ", err]
      mainLoop True
      
    Right ast -> do
      continue <- processCommand ast
      mainLoop continue