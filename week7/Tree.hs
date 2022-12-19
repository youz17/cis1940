data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

treeFold :: ret_type -> (ret_type -> val_type -> ret_type -> ret_type) -> Tree val_type -> ret_type
treeFold e _ Empty = e
treeFold e f (Node l v r) = f (treeFold e f l) v (treeFold e f r)