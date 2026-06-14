module Main (main) where

import System.IO (hFlush, stdout)

import Command
import Parser
import Shell

main :: IO ()
main = do
  mainLoop True

mainLoop :: Bool -> IO ()
mainLoop is_running = do 

  if not is_running
    then pure ()

    else do 

      putStr "$ "
      hFlush stdout

      input_raw <- getLine
      let command_raw = parseInput input_raw

      continue <- executeCommand (parseCommand command_raw)
      mainLoop continue