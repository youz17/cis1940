{-# OPTIONS_GHC -Wall #-}

module Calc where

import Data.Function ((&))
import Data.Map qualified as M
import ExprT (ExprT (..))
import Parser (parseExp)

eval :: ExprT -> Integer
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Lit a) = a

evalStr :: String -> Maybe Integer
evalStr s = parseExp Lit Add Mul s & fmap eval

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

--- expr var

data ExprVarT
  = VarLit Integer
  | VarAdd ExprVarT ExprVarT
  | VarMul ExprVarT ExprVarT
  | Var String
  deriving (Show, Eq)

class HasVars a where
  var :: String -> a

instance Expr ExprVarT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars ExprVarT where
  var = Var

type ExprVarType = (M.Map String Integer -> Maybe Integer)

type VarTable = M.Map String Integer

instance HasVars ExprVarType where
  var = M.lookup

instance Expr ExprVarType where
  lit :: Integer -> ExprVarType
  lit i _ = Just i
  add :: ExprVarType -> ExprVarType -> VarTable -> Maybe Integer
  add a b m = (+) <$> a m <*> b m
  mul :: ExprVarType -> ExprVarType -> ExprVarType
  mul a b m = (*) <$> a m <*> b m

withVars :: [(String, Integer)] -> ExprVarType -> Maybe Integer
withVars vs e = e $ M.fromList vs
