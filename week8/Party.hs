{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Party where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Tree (Tree (Node))
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL el fun) = GL (e : el) (empFun e + fun)

instance Semigroup GuestList where
  (GL el fun) <> (GL el' fun') = GL (el ++ el') (fun + fun')

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node value sub) = f value (map (treeFold f) sub)

-- (invite boss, not invite boss)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, withoutBoss)
  where
    withBoss = glCons boss (gls & map snd & mconcat)
    withoutBoss = gls & map (uncurry max) & mconcat

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun (treeFold nextLevel t)

maxFun1 :: GuestList
maxFun1 = maxFun testCompany

maxFun2 :: GuestList
maxFun2 = maxFun testCompany2

main :: IO ()
main =
  readFile "company.txt" <&> read <&> maxFun >>= format
  where
    format (GL el fun) = putStrLn ("Total fun: " ++ show fun) >> mconcat (map (putStrLn . empName) el)