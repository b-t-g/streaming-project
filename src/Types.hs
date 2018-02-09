-- Need for Lens
{-# LANGUAGE TemplateHaskell #-}
module Types where
import Data.Heap as Heap
import Data.Maybe as Maybe
import Control.Lens

data MeanInfo = MeanInfo
  {
    _size :: Double,
    _currentMean :: Double
  } deriving (Show)

data MedianInfo = MedianInfo
  {
    _currentMedian :: Double,
    _greaterThan :: Heap.MinHeap Double,
    _lessThan :: Heap.MaxHeap Double
  } deriving (Show)

data Process = Mean MeanInfo | Median MedianInfo deriving (Show)

makeLenses ''MeanInfo
makeLenses ''MedianInfo


class Processor i where
  -- | Take in a new element and find the median or mean
  process :: i -> Double -> i
  -- | Return the current median or mean
  info :: i -> Double
  show :: i -> String

instance Processor Process where
  process (Mean x) val = Mean (process x val)
  process (Median x) val = Median (process x val)
  info (Mean x) = info x
  info (Median x) = info x
  show (Mean x) = Types.show x
  show (Median x) = Types.show x


instance Processor MedianInfo where
  process info incomingVal =
    let updatedHeap = if incomingVal < _currentMedian info
                      then
                        over lessThan (insert incomingVal) info
                      else
                        over greaterThan (insert incomingVal) info
    in
       updateMedian $ balance updatedHeap

  info = _currentMedian

  show = Types.show


-- | For median finding, make sure the two heaps are balanced.
balance :: MedianInfo -> MedianInfo
balance info
  | abs (Heap.size (_greaterThan info) - Heap.size (_lessThan info)) <= 1 = info
  | Heap.size (_greaterThan info) > Heap.size (_lessThan info) + 1 =
    case viewHead (_greaterThan info) of
      Just a  -> let new = over lessThan (insert a) info
                 in over greaterThan (Heap.drop 1) new
      Nothing -> info
  | Heap.size (_lessThan info) > Heap.size (_greaterThan info) + 1 =
    case viewHead (_lessThan info) of
      Just a  -> let new = over greaterThan (insert a) info
                 in over lessThan (Heap.drop 1) new
      Nothing -> info


-- | Return the median after balancing the heaps.
updateMedian :: MedianInfo -> MedianInfo
updateMedian info
  | Heap.size (_greaterThan info) == Heap.size (_lessThan info) =
      let averageHeaps i t = (fromMaybe 0 (viewHead (_greaterThan i)) +
                      fromMaybe 1 (viewHead (_lessThan i))) / 2
      in
        over currentMedian (averageHeaps info) info
  | Heap.size (_greaterThan info) > Heap.size (_lessThan info) =
    over currentMedian ( \x -> fromMaybe 0 (Heap.viewHead $ _greaterThan info)) info
  | Heap.size (_greaterThan info) < Heap.size (_lessThan info) =
    over currentMedian (\x -> fromMaybe 0 (viewHead $ _lessThan info)) info

instance Processor MeanInfo where
  process info incomingVal = let updatedSize = over Types.size (+1) info
                                 in over currentMean (\x -> (_size info * x + incomingVal) / (_size info + 1)) updatedSize

  info = _currentMean

  show = Types.show
