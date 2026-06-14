module Main (main) where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
  mainLoop True

mainLoop :: Bool -> IO ()
mainLoop is_running = do 

  if not is_running
    
    then 
      mainLoop False

    else do 

      putStr "$ "
      hFlush stdout

      command <- getLine

      putStr (command ++ ": command not found\n")

      mainLoop True