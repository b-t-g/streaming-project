module Main where

import Lib
import System.Environment
import Types

main :: IO ()
main =
  do
    args <- getArgs
    let op = runOp (Operation (args !! 0)) in
      someFunc
