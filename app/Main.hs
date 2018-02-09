module Main where

import Lib
import System.Environment
import Types
import Data.Char
import Data.Heap as Heap
import Control.Exception

main :: IO ()
main =
  do
    args <- getArgs
    let op = case (map toLower (head args)) of
          "mean"   -> Right (Mean (MeanInfo 0 0))
          "median" -> Right (Median (MedianInfo 0 Heap.empty Heap.empty))
          _        -> Left (Mean (MeanInfo (-1) (-1)))
      in
      case op of
        Right x -> someFunc x
        Left y  -> putStrLn "Invalid option"
