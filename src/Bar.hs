{-# LANGUAGE DeriveGeneric #-}

module Bar
  ( Bar(..)
  , Alignment(..)
  ) where

import Dhall

data Alignment
  = ToTheLeft
  | ToTheRight
  | Centered
  deriving (Generic, Show)

data Bar = Bar
  { barLeftEnd :: Maybe Text
  , barRightEnd :: Maybe Text
  , barWidth :: Maybe Natural
  , alignment :: Maybe Alignment
  , separator :: Maybe Text
  , segments :: Vector Text
  } deriving (Generic, Show)
