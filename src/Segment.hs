{-# LANGUAGE DeriveGeneric #-}

module Segment
  ( Segment(..)
  ) where

import Dhall

data Segment = Segment
  { segmentLeftEnd :: Maybe Text
  , segmentRightEnd :: Maybe Text
  , segmentPaddingWidth :: Maybe Natural
  , segmentContent :: Text
  , segmentRefineContent :: Maybe (Text -> Text)
  } deriving (Generic)
