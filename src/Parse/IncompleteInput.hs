{-|
Module      : Parse.IncompleteInput
Description : Handles incomplete inputs and multiline continuations.
Author      : Michał Kośka

Detects and manages situations where user input is incomplete, such as 
unclosed quotes or trailing escape characters.
-}

module Parse.IncompleteInput where

import Types
import Parse.Tokeniser (tokeniseInput)
import System.Directory
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map

-- | Finds the longest common prefix among a list of strings.
commonPrefix :: String -> String -> String
commonPrefix (x:xs) (y:ys) 
  | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = ""

longestCommonPrefix :: [String] -> String
longestCommonPrefix []   = ""
longestCommonPrefix strs = foldl1 commonPrefix strs

-- | Splits path into directory prefix and base file name.
splitFilePath :: String -> (String, String)
splitFilePath token =
  let rev_text     = reverse token
      (name, path) = break (== '/') rev_text
  in (reverse path, reverse name)

-- | Appends trailing slashes to directory paths.
addSlashToDirectories :: [FilePath] -> IO [FilePath]
addSlashToDirectories = mapM $ \path -> do
    isDir <- doesDirectoryExist path
    return $ path ++ if isDir then "/" else ""

-- | Extracts the last word and the final token state from input.
getLastWordContext :: T.Text -> (T.Text, TokenState)
getLastWordContext input = 
  let (tokens, state) = tokeniseInput Map.empty input
      lastWord = fromMaybe "" (listToMaybe (reverse tokens))
  in (lastWord, state)

-- | Extracts the first word (command name) from input.
getFirstWord :: T.Text -> T.Text
getFirstWord input = 
  let (tokens, _) = tokeniseInput Map.empty input
  in fromMaybe "" (listToMaybe tokens)