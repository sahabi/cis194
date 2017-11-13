module Main where

import StringBuffer
import Editor
import JoinList
import Buffer
import Scrabble
import Sized

main = runEditor editor ((fromString . unlines)
        [ "This buffer is for notes you don't want to save, and for"
        , "evaluation of steam valve coeffiecients."
        , "To load a different file, type the character L followed"
        , "by the name of the file."
        ] :: (JoinList (Score, Size) String))

