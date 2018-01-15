{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where
import System.Environment
import System.IO
import Types
import Data.Conduit
import qualified Data.Conduit.Combinators as Cc

someFunc :: Op -> IO ()
someFunc op =
  runConduit $
  Cc.stdin
  .| Cc.mapM_ print
