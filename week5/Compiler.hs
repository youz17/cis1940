{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

import StackVM (Program)

-- todo: 等学完 Monad, Functor 之类再来看看
compile :: String -> Maybe Program
compile = undefined

-- compile "1+2*3"
-- Push 2, Push 3, Mul, Push 1, Add