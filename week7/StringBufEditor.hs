module Main where

import Buffer
import Editor
import JoinList (JoinListBuffer)

jlFromLines :: [String] -> JoinListBuffer
jlFromLines = fromString . unlines

main =
  runEditor editor $
    jlFromLines
      [ "This buffer is for notes you don't want to save, and for",
        "evaluation of steam valve coefficients.",
        "To load a different file, type the character L followed",
        "by the name of the file."
      ]
