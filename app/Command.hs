module Command where

import System.Environment
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Control.Exception
import Data.Maybe (listToMaybe)
import Control.Monad


-- typ danych Command
data Command = BuiltIn BuiltInCommand
  | External T.Text [T.Text]
  | Blank

data BuiltInCommand = Exit
  | Echo [T.Text]
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

getMatchingExecutables :: T.Text -> IO [T.Text]
getMatchingExecutables prefix = do

  dirs <- getPath
  -- lambda function as an argument \variable -> do ... 
  results_list <- forM dirs $ \dir_text -> do
      let dir_str = T.unpack dir_text
      exists <- doesDirectoryExist dir_str
    
      if exists
        then do
          attempt <- try (listDirectory dir_str) :: IO (Either SomeException [FilePath])
          -- without attempt, program doesnt work as soon as it comes across permission denial    
          case attempt of

            Left _error -> return []
            Right commands -> do
              let commands_text = map T.pack commands
              return $ filter (T.isPrefixOf prefix) commands_text
        else
          return []
  
  return $ concat results_list