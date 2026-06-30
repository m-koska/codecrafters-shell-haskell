module Main (main) where

import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho)
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Command
import Parser
import Tokeniser
import Shell
import InputHandler
import System.OsPath
import System.Posix (homeDirectory)

main :: IO ()


main = do

-- initial setup for stdin and stdout
-- echo - displaying what comes from stdin
-- buffering - wheather the programm processes for example entire lines or every key separately

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  T.IO.putStr "$ "
  mainLoop ""
  return ()


