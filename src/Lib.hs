{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where
import Types
import Conduit
import Data.ByteString
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.Lift
import qualified Data.Conduit.Combinators as Cc
import Data.Maybe
import System.Environment
import System.IO

someFunc :: (Processor p, Show p) => p -> IO ()
someFunc op =
  runConduit $
  Cc.stdin
  .| Conduit.mapC (\x -> read (fromMaybe "" (fromByteString x)) :: Double)
  .| Conduit.mapC (process op)
  .| Cc.mapM_ (\x -> (print $ info x) >> someFunc x)
