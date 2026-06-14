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

      input_raw <- getLine
      let command_raw = parseInput input_raw

      continue <- executeCommand (parseCommand command_raw)
      mainLoop continue


-- typ danych Command
data Command = Blank
  | Unknown String 
  | Echo 
  | Exit

-- Zwraca stringa funkcji i stringa jej argumentów
-- albo nic jak jest źle wpisane 
parseInput :: String -> Maybe (String, String)
parseInput input = case words input of
  []       -> Nothing
  (c:args) -> Just (c, unwords args)

parseCommand :: Maybe (String, String) -> (Command, String)
parseCommand input_command = case input_command of
  Nothing     -> (Blank, "")
  Just ("exit", _)     -> (Exit, "")
  Just ("echo", args)  -> (Echo, args)
  Just (unknown_command, args) -> (Unknown unknown_command, args)

-- Wykonywanie komend wbudowanych + TODO: komendy niewbudowane
executeCommand :: (Command, String) -> IO Bool

executeCommand (Blank, _) = return True

executeCommand (Echo, args) = do 
  putStrLn args
  return True

executeCommand (Exit, _) = return False

executeCommand (Unknown unknown_command, args) = do 
  putStrLn (unknown_command ++ ": command not found")
  return True