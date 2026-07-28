{-|
Module      : Types
Description : Core domain models and type definitions.
Author      : Michał Kośka

Contains the foundational data structures used across the shell, including 
the Abstract Syntax Tree (AST), Command definitions, and TokenState.
-}

module Types where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Process (ProcessHandle)

data TokenState
  = NormalText
  | SingleQuoteText
  | DoubleQuotedText
  | BackslashText
  | BackslashQuotedText

-- | Represents the primary execution unit parsed from the user input.
-- It categorizes whether the target command runs natively or externally.
data Command
  = BuiltIn BuiltInCommand   -- ^ Native shell commands executed in-process.
  | External T.Text [T.Text] -- ^ External binary name found via $PATH and its raw arguments.
  | Blank                    -- ^ No-op placeholder when the user just hits Enter.

-- | Set of commands handled internally by the shell instead of spawning a new process.
data BuiltInCommand
  = Exit          -- ^ Terminates the shell session.
  | Echo [T.Text] -- ^ Prints arguments separated by spaces to stdout.
  | Type [T.Text] -- ^ Checks if a command name is a builtin or an executable in $PATH.
  | Pwd           -- ^ Prints the absolute path of the current working directory.
  | Cd T.Text     -- ^ Changes the working directory.
  | Complete [T.Text]
  | Jobs
  | History [T.Text]

-- | Abstract Syntax Tree (AST) representing the structure of a command line sequence.
-- Handles wrapping command executions with file redirections.
data AST
  = ExecNode Command                                      -- ^ Terminal leaf node representing a command execution.
  | RedirectNode RedirectionType AST FilePath WriteMethod -- ^ Wraps an AST node to capture and redirect its output stream.
  | BackgroundJobNode AST

-- | Specifies which output descriptor is captured for redirection.
data RedirectionType
  = StandardRedirection -- ^ Redirects standard output (stdout / file descriptor 1).
  | ErrorRedirection    -- ^ Redirects standard error (stderr / file descriptor 2).

-- | Defines how data is written to the destination file during redirection.
data WriteMethod
  = TruncateMethod -- ^ Overwrites the target file content (using the '>' operator).
  | AppendMethod   -- ^ Appends data to the end of the target file (using the '>>' operator).

-- | Tracks the state of the last pressed key to support progressive tab-completion logic.
data KeyType
  = TabKey   -- ^ Indicates the immediate previous action was a <TAB> key press.
  | OtherKey -- ^ Represents any other standard character, backspace, or newline input.

data JobInfo = JobInfo 
  { job_handle :: ProcessHandle
  , job_cmd :: T.Text
  }

data ShellState = ShellState 
  { buffer            :: String
  , prev_key          :: KeyType
  , completions       :: Map.Map T.Text FilePath
  , bg_jobs           :: Map.Map Int JobInfo
  , next_job_id       :: Int
  , is_next_cmd_in_bg :: Bool
  , history           :: [T.Text]
  , history_index     :: Int
  , history_write_idx :: Int
  }

type Shell a = StateT ShellState IO a