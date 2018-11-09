{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Segment
  ( Segment(..)
  , printSegment
  ) where

import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Dhall hiding (maybe)

data Segment = Segment
  { segmentLeftEnd :: Maybe Text
  , segmentRightEnd :: Maybe Text
  , segmentPaddingWidth :: Maybe Natural
  , segmentContent :: Text
  , segmentRefineContent :: Maybe Text
  } deriving (Generic)

refiningFunctions =
  M.fromList [("kanji", romanToKanji)] :: M.Map T.Text (T.Text -> T.Text)

textToInt :: T.Text -> Int
textToInt = fromIntegral . read . T.unpack

romanToKanji :: T.Text -> T.Text
romanToKanji number =
  fromMaybe number $ do
    let index = textToInt number
    let kanjiNumbers = "〇一二三四五六七八九十"
    guard (index < T.length kanjiNumbers)
    let kanjiCharacter = T.index kanjiNumbers index
    return (T.singleton kanjiCharacter)

printSegment :: Segment -> Text
printSegment segment =
  foldl1
    T.append
    (catMaybes
       ([ segmentLeftEnd segment
        , flip T.replicate " " . fromIntegral <$> segmentPaddingWidth segment
        , return
            (fromMaybe
               id
               (segmentRefineContent segment >>= flip M.lookup refiningFunctions)
               (segmentContent segment))
        , flip T.replicate " " . fromIntegral <$> segmentPaddingWidth segment
        , segmentRightEnd segment
        ]))
