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

import CLI.InputHandler
import Types

main :: IO ()
main = do

  -- initial setup for stdin and stdout
  -- echo - displaying what comes from stdin
  -- buffering - wheather the programm processes for example entire lines or every key separately
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  -- run the REPL
  T.IO.putStr "$ "
  mainLoop ShellState {buffer = "", prev_key = OtherKey}
  return ()