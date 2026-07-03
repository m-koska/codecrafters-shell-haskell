{-|
Module      : CLI.CompletionHandler
Description : Provides tab-completion functionality.
Author      : Michał Kośka

Responsible for resolving partial inputs into full command names, file paths, 
or directories when the user triggers autocompletion.
-}

module CLI.CompletionHandler where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.List

import Types
import Exec.Command
import Parse.Parser
import Exec.Shell
import Parse.Tokeniser
import Parse.IncompleteInput
import Control.Exception
import System.Directory
import Control.Monad (when)
import System.Posix (isDirectory)

handleCompletion :: String -> KeyType -> IO (String, KeyType)
handleCompletion input_buffer prev_key = do
  let (token_to_complete, final_state) = getLastWordContext input_buffer
  
  (text_to_print, new_buffer, new_key) <- 
    if length input_buffer > length token_to_complete
      then completeFilename input_buffer token_to_complete prev_key final_state 
      else completeCommand input_buffer prev_key

  putStr text_to_print
  return (new_buffer, new_key)

data CompletionResult 
  = CompletionFile
  | CompletionDirectory
  | CompletionCommand
  | CompletionNone

completeFilename :: String -> String -> KeyType -> TokenState -> IO (String, String, KeyType)
completeFilename input token_to_complete prev_key final_state  = do
  
  let (dir, file) = splitFilePath token_to_complete
      directory_path = if null dir then "." else dir
      file_prefix = file

  attempt <- try (listDirectory directory_path) :: IO (Either SomeException [FilePath])

  case attempt of 
    Left _ -> do
      return ("\x07", input, TabKey) 

    Right files -> do
      let matches = sort $ filter (file_prefix `isPrefixOf`) files
      case (matches, prev_key) of
        ([], _) -> do
          return ("\x07", input, TabKey) 
        
        ([single_match], _) -> do
          let full_path = if null dir then single_match else dir ++ single_match
          is_dir <- doesDirectoryExist full_path

          let to_put = drop (length file_prefix) single_match
              ending_char = if is_dir
                then "/"
                else case final_state of
                  SingleQuoteText  -> "' "
                  DoubleQuotedText -> "\" "
                  _                -> " "
          
          return (to_put ++ ending_char, input ++ to_put ++ ending_char, OtherKey)
        
        (_, TabKey) -> do
          matches_with_slashes <-addSlashToDirectories matches
          let joined_matches = intercalate "  " matches_with_slashes
          return ("\n" ++ joined_matches ++ "\n$ " ++ input, input, TabKey)
          
        (_, _) -> do
          return ("\x07", input, TabKey) 

completeCommand :: String -> KeyType -> IO (String, String, KeyType)
completeCommand input prev_key = do

  let input_text = T.pack input
      matching_builtins = filter (T.isPrefixOf input_text) builtins
  
  matching_ext <- getMatchingExecutables input_text
  
  let matches = Data.List.sort $ Data.List.nub (matching_builtins ++ matching_ext)   
  
  case (matches, prev_key) of
    ([], _) -> do
      return ("\x07", input, OtherKey)
          
    ([single_match], _) -> do

      let to_put = T.unpack $ T.drop (length input) single_match 
      return (to_put ++ " ", input ++ to_put ++ " ", OtherKey)

    (_, _) -> do
      let matches_str = map T.unpack matches
          lcp = longestCommonPrefix matches_str
      
      if length lcp > length input
        then do
          let to_put = drop (length input) lcp
          return (to_put, lcp, OtherKey)

        else do
          case prev_key of
            TabKey -> do

              let joined_matches = T.unpack $ T.intercalate "  " matches
                  screen_output = "\n" ++ joined_matches ++ "\n$ " ++ input
              
              return (screen_output, input, TabKey)
              
            OtherKey -> do
              return ("\x07", input, TabKey)