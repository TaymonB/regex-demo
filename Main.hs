module Main where

import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified StateMachines
import qualified RegexParsing
    
main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [pattern,str] ->
        case RegexParsing.parseRegex pattern of
          Left error ->
              IO.hPrint IO.stderr error >> Exit.exitWith (Exit.ExitFailure 2)
          Right machine ->
              Exit.exitWith $ if StateMachines.matches machine str
                              then Exit.ExitSuccess else Exit.ExitFailure 1
    _ -> do
         progName <- Environment.getProgName
         IO.hPutStrLn IO.stderr ("usage: " ++ progName ++ " pattern string")
         Exit.exitWith $ Exit.ExitFailure 2
