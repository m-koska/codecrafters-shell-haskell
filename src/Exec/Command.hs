{-|
Module      : Exec.Command
Description : Resolution and execution of external commands.
Author      : Michał Kośka

Handles the resolution of executable paths by searching through the system's 
$PATH environment variable and managing the execution of those external programs.
-}

module Exec.Command where

import Control.Exception
import Control.Monad
import Data.Maybe (listToMaybe)
import System.Directory
import System.Environment
import System.FilePath

import qualified Data.Text as T

builtins :: [T.Text]
builtins = ["exit", "echo", "type", "pwd", "cd", "complete", "jobs", "history", "declare"]

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
          -- because its calling listDirectory   
          case attempt of

            Left _error -> return []
            Right commands -> do
              let commands_text = map T.pack commands
              return $ filter (T.isPrefixOf prefix) commands_text
        else
          return []
  
  return $ concat results_list