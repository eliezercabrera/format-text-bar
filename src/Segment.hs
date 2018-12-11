{-# LANGUAGE DeriveAnyClass #-}
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
import qualified Safe as Safe

data Segment = Segment
  { segmentLeftEnd :: Maybe Text
  , segmentRightEnd :: Maybe Text
  , segmentPaddingWidth :: Natural
  , segmentContent :: Text
  , segmentRefineContent :: Maybe Text
  } deriving (Generic, Interpret, Inject)

refiningFunctions :: M.Map T.Text (T.Text -> T.Text)
refiningFunctions =
  let indexEntry name = (name, stylizeIndex name)
   in M.fromList
        (fmap (indexEntry . fst) (M.toList numberSystems) ++
         [("command", id), ("shortenPath", shortenPath), ("title", id)])

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

initialsOfDirectory :: T.Text -> T.Text
initialsOfDirectory directoryName
  -- Should not happen.
  | T.null directoryName = ""
  | T.head directoryName == '.' = T.cons '.' (helper (T.tail directoryName))
  | otherwise = helper directoryName
  where
    helper processedDirectory =
      T.foldl
        selectCharacters
        (T.singleton (T.head processedDirectory))
        (T.tail processedDirectory)
    selectCharacters currentString currentCharacter
      | isDelimiter currentCharacter = T.snoc currentString currentCharacter
      | isDelimiter (T.last currentString) =
        T.snoc currentString currentCharacter
      | otherwise = currentString
    delimiters = "-_ "
    isDelimiter character = not (T.null (T.filter (== character) delimiters))

shortenPath :: T.Text -> T.Text
shortenPath path =
  Safe.findJust
    ((<= maxWidth) . T.length)
    [ homeSubstituted
    , centerDirectoriesShortened
    , allDirectoriesShortened
    , firstAndLast
    ]
  where
    maxWidth = 40
    delimiter = "/"
    splitPath = dropWhile T.null (T.splitOn delimiter path)
    homeSubstituted =
      fromMaybe path $ do
        topLevelDirectory <- Safe.headMay splitPath
        guard (topLevelDirectory == "home")
        return (T.append "~/" (T.intercalate delimiter (drop 2 splitPath)))
    splitHome = T.splitOn delimiter homeSubstituted
    centerDirectoriesShortened =
      fromMaybe homeSubstituted $ do
        top <-
          do trueTop <- Safe.headMay splitHome
             if trueTop == "~"
               then Safe.atMay splitHome 1
               else return trueTop
        bottom <- Safe.lastMay splitHome
        center <- Safe.tailMay =<< Safe.initMay splitHome
        return
          (T.intercalate
             delimiter
             (["~", top] ++ fmap initialsOfDirectory center ++ [bottom]))
    allDirectoriesShortened =
      T.intercalate delimiter (fmap initialsOfDirectory splitHome)
    firstAndLast =
      T.take maxWidth . fromMaybe homeSubstituted $ do
        top <- Safe.headMay splitHome
        bottom <- Safe.lastMay splitHome
        return (T.intercalate delimiter [top, "...", bottom])

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
