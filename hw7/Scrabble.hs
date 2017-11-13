{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char
import Data.Monoid

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c 
    | toUpper c `elem` ['A','E','I','O','U','L','N','S','T','R'] = Score 1
    | toUpper c `elem` ['D','G'] = Score 2
    | toUpper c `elem` ['B','C','M','P'] = Score 3
    | toUpper c `elem` ['F','H','V','W','Y'] = Score 4
    | toUpper c == 'K' = Score 5
    | toUpper c `elem` ['J','X'] = Score 8
    | toUpper c `elem` ['Q','Z'] = Score 10
score _ = 0

scoreString :: String -> Score
scoreString = foldl (+) 0 . map score
