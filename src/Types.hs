module Types where
import Data.Char

newtype Input = Operation String

data Op = Mean | Median

runOp :: Input -> Op
runOp (Operation op) =
  case map toLower op of
    "mean"    -> Mean
    "average" -> Mean
    "median"  -> Median
