module Main where

import CLI.CliOptions ( dispatch, help )
import CLI.Entrypoint ( entrypoint )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  if null args then do
      help []
  else
    do
    let command = head args
    let remainingArgs = tail args

    entrypoint (lookup command dispatch) remainingArgs