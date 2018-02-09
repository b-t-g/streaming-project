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
import Numeric
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.ByteString as Bsp

someFunc :: (Processor p, Show p) => p -> IO ()
someFunc op =
  runConduit $
  Cc.stdin
  .| Conduit.mapC (\x -> parse parseInput "parseInput" x)
  .| Conduit.mapC (
  \x -> case x of
          Left y   -> op
          Right z -> process op z
  )
  .| Cc.mapM_ (\x -> (print $ info x) >> someFunc x)


parseInput :: Bsp.Parser Double
parseInput =
  many1 digit >>= \prefix ->
  optional (char '.') >>
  many digit >>= \suffix ->
  let floatedNumber = prefix ++ "." ++ suffix in
    return (fst $ Prelude.head (readFloat floatedNumber))
