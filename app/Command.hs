module Command where

import System.Environment (getEnv)
import qualified Data.Text as T

-- typ danych Command
data Command = BuiltIn BuiltInCommand
  | External T.Text T.Text
  | Blank

data BuiltInCommand = Exit
  | Echo T.Text
  | Type [T.Text]

builtins :: [T.Text]
builtins = map T.pack ["exit", "echo", "type"]

isBuiltIn :: T.Text -> Bool
isBuiltIn name =
  name `elem` builtins

getPath :: IO [T.Text]
getPath =
  fmap (T.splitOn ":" . T.pack) (getEnv "PATH")