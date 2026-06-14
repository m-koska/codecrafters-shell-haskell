module Command where

-- typ danych Command
data Command = BuiltIn BuiltInCommand
  | External String String
  | Blank

data BuiltInCommand = Exit
  | Echo String
  | Type [String]

isBuiltIn :: String -> Bool
isBuiltIn name =
  name `elem` ["exit", "echo", "type"]