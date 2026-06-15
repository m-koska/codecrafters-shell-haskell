module Command where

import System.Environment
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)

-- typ danych Command
data Command = BuiltIn BuiltInCommand
  | External T.Text [T.Text]
  | Blank

data BuiltInCommand = Exit
  | Echo T.Text
  | Type [T.Text]
  | Pwd
  | Cd T.Text

builtins :: [T.Text]
builtins = map T.pack ["exit", "echo", "type", "pwd", "cd"]

isBuiltIn :: T.Text -> Bool
isBuiltIn name =
  name `elem` builtins

getPath :: IO [T.Text]
getPath =
  fmap (T.splitOn ":" . T.pack) (getEnv "PATH")

-- T.unpack - żeby dostać Stringa z Text
getCommand :: FilePath -> IO (Maybe FilePath)
getCommand path = do
  
  dirs <- fmap (map T.unpack) getPath

  let commands = map (</> path) dirs 

  found <- filterM isExecutable commands

  return $ listToMaybe found

  where
    isExecutable :: FilePath -> IO Bool
    isExecutable file = do

      exist <- doesFileExist file

      if exist
        then executable <$> getPermissions file  
        else return False