module Main (main) where

import System.IO (hFlush, stdout)

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

      command_raw <- getLine

      continue <- executeCommand (parseCommand command_raw)
      mainLoop continue


-- typ danych Command
data Command = Exit | Unknown String 

-- Parser do komend - sprawdza, czy mamy taką komendę, czy nie 
parseCommand :: String -> Command
parseCommand "exit" = Exit
parseCommand x = Unknown x

-- Wykonywanie komend wbudowanych + TODO: kom,komendy niewbudowane
executeCommand :: Command -> IO Bool

executeCommand Exit = return False

executeCommand (Unknown unknown_command) = do 
  putStrLn (unknown_command ++ ": command not found")
  return True