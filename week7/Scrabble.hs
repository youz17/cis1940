{-# OPTIONS_GHC -Wall #-}

module Scrabble (Score (Score), score, scoreString) where

import Data.Char (toUpper)
import Data.Set qualified as S

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

createScoreSet :: [Char] -> S.Set Char
createScoreSet cl = S.fromList (cl ++ map toUpper cl)

oneScoreSet :: S.Set Char
oneScoreSet = createScoreSet "aeilnorstu"

twoScoreSet :: S.Set Char
twoScoreSet = createScoreSet "dg"

threeScoreSet :: S.Set Char
threeScoreSet = createScoreSet "bcmp"

fourScoreSet :: S.Set Char
fourScoreSet = createScoreSet "fhvwy"

fiveScoreSet :: S.Set Char
fiveScoreSet = createScoreSet "k"

eightScoreSet :: S.Set Char
eightScoreSet = createScoreSet "jx"

tenScoreSet :: S.Set Char
tenScoreSet = createScoreSet "qz"

score :: Char -> Score
score c
  | S.member c oneScoreSet = 1
  | S.member c twoScoreSet = 2
  | S.member c threeScoreSet = 3
  | S.member c fourScoreSet = 4
  | S.member c fiveScoreSet = 5
  | S.member c eightScoreSet = 8
  | S.member c tenScoreSet = 10
  | otherwise = 0

scoreString :: String -> Score
scoreString = sum . map score
