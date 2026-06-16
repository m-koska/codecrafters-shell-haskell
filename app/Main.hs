module Main (main) where

import System.IO (hFlush, stdout)
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Command
import Parser
import Shell
import System.OsPath
import System.Posix (homeDirectory)

main :: IO ()
main = do
  home_directory <- getHomeDirectory
  mainLoop True home_directory
  return ()

mainLoop :: Bool -> FilePath -> IO ()
mainLoop is_running current_directory = do 

  if not is_running
    then pure ()

    else do 

      putStr "$ "
      hFlush stdout
      
      input_raw <- getLine
      let command_raw = parseInput input_raw

      continue <- executeCommand $ parseCommand command_raw
      mainLoop continue current_directory