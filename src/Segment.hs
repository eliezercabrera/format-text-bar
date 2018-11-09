{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , segmentPaddingWidth :: Natural
  , segmentContent :: Text
  , segmentRefineContent :: Maybe Text
  } deriving (Generic)

refiningFunctions :: M.Map T.Text (T.Text -> T.Text)
refiningFunctions =
  let indexEntry name = (name, stylizeIndex name)
   in M.fromList
        (fmap (indexEntry . fst) (M.toList numberSystems) ++
         [("command", id), ("shortenPath", id), ("title", id)])

textToInt :: T.Text -> Int
textToInt = fromIntegral . read . T.unpack

numberSystems :: M.Map T.Text T.Text
numberSystems =
  M.fromList
    [ ("kanji", "一二三四五六七八九十")
    , ("darkCircled", "➊➋➌➍➎➏➐➑➒➓")
    , ("lightCircled", "➀➁➂➃➄➅➆➇➈➉")
    , ("lightDoubleCircled", "⓵⓶⓷⓸⓹⓺⓻⓼⓽⓾")
    , ("parenthesized", "⑴⑵⑶⑷⑸⑹⑺⑻⑼⑽")
    ]

stylizeIndex :: T.Text -> T.Text -> T.Text
stylizeIndex numberSystemName number =
  fromMaybe number $ do
    let index = textToInt number
    numberSystem <- M.lookup numberSystemName numberSystems
    guard (index < T.length numberSystem - 1)
    let stylizedCharacter = T.index numberSystem (index - 1)
    return (T.singleton stylizedCharacter)

printSegment :: Segment -> Text
printSegment segment =
  foldl1
    T.append
    (catMaybes
       ([ segmentLeftEnd segment
        , return . flip T.replicate " " . fromIntegral $
          segmentPaddingWidth segment
        , return
            (fromMaybe
               id
               (segmentRefineContent segment >>= flip M.lookup refiningFunctions)
               (segmentContent segment))
        , return . flip T.replicate " " . fromIntegral $
          segmentPaddingWidth segment
        , segmentRightEnd segment
        ]))
