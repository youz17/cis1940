{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Ord (Down (Down))
import GHC.Utils.BufHandle (bFlush)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  let ats = attackers bf
  let dfs = defenders bf
  let atUnit = if ats > 3 then 3 else ats - 1
  let dfUnit = if dfs >= 2 then 2 else dfs
  let genValues n = replicateM n die <&> sortOn Down
  at <- genValues atUnit
  df <- genValues dfUnit
  let (aDieCount, dDieCount) = battle' (at, df)
  return $ Battlefield (ats - aDieCount) (dfs - dDieCount)
  where
    battle' v = case v of
      (a : as, b : bs) ->
        let (aDieCount, dDieCount) = battle' (as, bs)
         in if a > b then (aDieCount, dDieCount + 1) else (aDieCount + 1, dDieCount)
      ([], _) -> (0, 0)
      (_, []) -> (0, 0)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  newBf <- battle bf
  let remainDfs = defenders newBf
  let remainAts = attackers newBf
  if remainDfs <= 0 || remainAts < 2
    then return newBf
    else invade newBf

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  res <- replicateM 1000 (invade bf)
  let winCount = fromIntegral $ length $ filter id $ map attWin res
  return $ winCount / 1000
  where
    attWin bf = defenders bf == 0

run :: (Show a) => (Battlefield -> Rand StdGen a) -> Battlefield -> IO ()
run f bf = evalRandIO (f bf) >>= print
