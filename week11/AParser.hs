{- CIS 194 HW 10
   due Monday, 1 April
-}
{-# OPTIONS_GHC -Wall #-}

module AParser where

import Control.Applicative (Alternative)
import Data.Char
import GHC.Base (Alternative (..))

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p) -- Parser (\s -> (first f) <$> p s)

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))

  -- p1 <*> p2 = Parser (\s ->
  --   case runParser p1 s of
  --     Nothing -> Nothing
  --     Just (f, ss) ->
  --       case runParser p2 ss of
  --         Nothing -> Nothing
  --         Just (a, sss) -> Just (f a, sss))
  p1 <*> p2 = Parser pp
    where
      pp s = runParser p1 s >>= helper
      -- helper (f, s) = runParser p2 s <&> first f
      helper (f, s) = runParser (f <$> p2) s

-- 网上抄的解法
-- pf <*> px = Parser f
--   where f str = case runParser pf str of
--           Nothing       -> Nothing
--           Just (fr, sr) -> fmap (first fr) (runParser px sr)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ c -> [a, c]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

intOrUppercase :: Parser ()
intOrUppercase = (ignore <$> posInt) <|> (ignore <$> satisfy isUpper)
  where
    ignore = const ()
