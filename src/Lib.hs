{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
  .| Conduit.mapC (parse parseInput "parseInput")
  .| Conduit.mapC (
  \case
          Left y   -> Nothing
          Right z  -> Just $ process op z
  )
  .| Cc.mapM_ (
  \case
      Just y  -> print (info y) >> someFunc y
      Nothing -> print "Invalid input, omitting value" >> someFunc op)


parseInput :: Bsp.Parser Double
parseInput =
  many1 digit >>= \prefix ->
  optional (char '.') >>
  many digit >>= \suffix ->
  let floatedNumber = prefix ++ "." ++ suffix in
    return (fst $ Prelude.head (readFloat floatedNumber))
