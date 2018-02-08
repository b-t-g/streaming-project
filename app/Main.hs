module Main where

import Lib
import System.Environment
import Types
import Data.Heap as Heap

main :: IO ()
main =
  do
    args <- getArgs
    let op = case (head args) of
          "Mean" -> Mean (MeanInfo 0 0)
          "Median" -> Median (MedianInfo 0 Heap.empty Heap.empty)
      in
      someFunc op
