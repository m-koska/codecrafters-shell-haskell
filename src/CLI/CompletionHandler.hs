{-|
Module      : CLI.CompletionHandler
Description : Provides tab-completion functionality.
Author      : Michał Kośka

Responsible for resolving partial inputs into full command names, file paths, 
or directories when the user triggers autocompletion.
-}

module CLI.CompletionHandler where

import Types
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS
import Exec.Command
import Parse.Parser
import Exec.Shell
import Parse.Tokeniser
import Parse.IncompleteInput

import System.Directory
import System.Posix
import System.Process

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.List
import qualified Data.Map as Map
import qualified Control.Exception

handleCompletion :: Shell ()
handleCompletion = do
  state <- get
  
  let input_buffer = buffer state
      previous_key = prev_key state

      (token_to_complete, final_state) = getLastWordContext input_buffer

      cmd = getFirstWord input_buffer

  (text_to_print, new_buffer, new_key) <- 
    if length input_buffer > length token_to_complete
      then case Map.lookup cmd (completions state) of
          Just script -> liftIO $ 
            completeFromScript 
              input_buffer token_to_complete previous_key final_state script

          Nothing     -> liftIO $
            completeFilename 
              input_buffer token_to_complete previous_key final_state 
      
      else liftIO $ 
        completeCommand input_buffer previous_key

  liftIO $ putStr text_to_print
  modify $ \s ->
    s 
    {
      buffer = new_buffer
    , prev_key = new_key 
    }

data CompletionResult 
  = CompletionFile
  | CompletionDirectory
  | CompletionCommand
  | CompletionNone

completeFromScript :: String -> String -> KeyType -> TokenState -> FilePath -> IO (String, String, KeyType)
completeFromScript input token_to_complete prev_key final_state script = do

  completion_out <- readProcess script [] ""
  
  let completions = lines completion_out
      matches     = filter (isPrefixOf token_to_complete) completions

  case (matches, prev_key) of
    ([], _) -> do
      return ("\x07", input, OtherKey)

    ([single_match], _) -> do
      let to_put = drop (length token_to_complete) single_match 
      return (to_put ++ " ", input ++ to_put ++ " ", OtherKey)

    (_, _) -> do
      let lcp = longestCommonPrefix matches
      
      if length lcp > length token_to_complete
        then do
          let to_put = drop (length token_to_complete) lcp
          return (to_put, input ++ to_put, OtherKey)

        else do
          case prev_key of
            TabKey -> do

              let joined_matches = intercalate "  " matches
                  screen_output = "\n" ++ joined_matches ++ "\n$ " ++ input
              
              return (screen_output, input, TabKey)
              
            OtherKey -> do
              return ("\x07", input, TabKey)

completeFilename :: String -> String -> KeyType -> TokenState -> IO (String, String, KeyType)
completeFilename input token_to_complete prev_key final_state  = do
  
  let (dir, file) = splitFilePath token_to_complete
      directory_path = if null dir then "." else dir
      file_prefix = file

  attempt <- Control.Exception.try (listDirectory directory_path) :: IO (Either Control.Exception.SomeException [FilePath])

  case attempt of 
    Left _ -> do
      return ("\x07", input, TabKey) 

    Right files -> do
      let matches = sort $ filter (file_prefix `isPrefixOf`) files
      case matches of
        [] -> do
          return ("\x07", input, TabKey)
        
        [single_match] -> do
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

        _ -> do
          let lcp = longestCommonPrefix matches

          if length lcp > length file_prefix
            then do
              let string_delta = drop (length file_prefix) lcp
              return (string_delta, input ++ string_delta, OtherKey)

            else do
              case prev_key of
                TabKey -> do
                  matches_with_slashes <-addSlashToDirectories matches

                  let joined_matches = intercalate "  " matches_with_slashes
                      screen_output = "\n" ++ joined_matches ++ "\n$ " ++ input
                  
                  return (screen_output, input, TabKey)
                  
                OtherKey -> do
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