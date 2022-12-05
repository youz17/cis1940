type Peg = String

type Move = (String, String) -- (from, to)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = if n <= 0 then [] else hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoi' :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 1 a b _ _ = [(a, b)]
hanoi' 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi' n a b c d =
  if n <= 0
    then []
    else hanoi' (n - 2) a c b d ++ hanoi 2 a b d ++ hanoi' (n - 2) c b a d