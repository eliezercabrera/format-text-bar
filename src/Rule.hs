{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Rule
  ( Rule(..)
  , TextComparator(..)
  , BarOperation(..)
  , processRule
  ) where

import Bar
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Vector as V
import Dhall hiding (maybe)
import Segment

data TextComparator
  = Equals { needle :: Text }
  | StartsWith { needle :: Text }
  | EndsWith { needle :: Text }
  | Includes { needle :: Text }
  | Regex { needle :: Text }
  deriving (Generic, Interpret)

data BarOperation
  = DeleteSegment { segmentName :: Text }
  | AddSegment { segmentName :: Text }
  | ReplaceBar { segmentNames :: V.Vector Text }
  | RefineSegment { segmentName :: Text
                  , refineFunction :: Text }
  deriving (Generic, Interpret)

data Condition = Condition
  { conditionSegmentName :: Text
  , operator :: TextComparator
  , ignoreCase :: Bool
  } deriving (Generic, Interpret)

data Rule = Rule
  { conditions :: V.Vector (V.Vector Condition)
  , effects :: V.Vector BarOperation
  } deriving (Generic, Interpret)

processCondition :: Condition -> V.Vector Segment -> Bool
processCondition (Condition conditionSegmentName operator ignoreCase) processedBar =
  fromMaybe False $ do
    let hasSegment segment = Segment.segmentName segment == conditionSegmentName
    segment <- V.find hasSegment processedBar
    return $ do
      case operator of
        (Equals key) -> (segmentContent segment == key)

processBarOperation :: BarOperation -> V.Vector Segment -> V.Vector Segment
processBarOperation (DeleteSegment nameOfSegment) processedBar =
  fromMaybe processedBar $ do
    return (V.filter ((/= nameOfSegment) . Segment.segmentName) processedBar)

processRule :: Rule -> V.Vector Segment -> V.Vector Segment
processRule (Rule conditions effects) processedBar =
  if V.or (fmap (V.all (flip processCondition processedBar)) conditions)
    then V.foldl (flip processBarOperation) processedBar effects
    else processedBar
