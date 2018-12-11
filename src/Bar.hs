{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bar
  ( Bar(..)
  , Alignment(..)
  , printBar
  ) where

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Dhall hiding (maybe)
import Segment

data Alignment
  = ToTheLeft
  | ToTheRight
  | Centered
  deriving (Generic, Interpret)

data Bar = Bar
  { barLeftEnd :: Maybe Text
  , barRightEnd :: Maybe Text
  , barWidth :: Maybe Natural
  , alignment :: Alignment
  , separator :: Maybe Text
  , barSegments :: Vector Text
  } deriving (Generic, Interpret)

printBar :: V.Vector Segment -> Bar -> Text
printBar segments bar =
  foldl1
    T.append
    (catMaybes
       [ barLeftEnd bar
       , return
           (T.intercalate
              (fromMaybe "" (separator bar))
              (V.toList (V.map printSegment segments)))
       , barRightEnd bar
       ])
