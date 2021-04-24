module Main where

{-
    Update "Main.hs" so that it implements a program which breaks its input up into
    lines, reverses each line (for additional security) and applies the Caesar
    cipher with a shift width of 5 to it, then prints out the encrypted lines.
    
    (Hint: look into the `lines` and `unlines` functions).
-}

import Data.Char
caesar :: Int -> String -> String
caesar _ [] = []
caesar n (x:xs) = (if isLetter x then encrypt x else x) : caesar n xs
    where encrypt x = n2l ((l2n x + n) `mod` 26)
        l2n c = ord (toUpper c) - ord 'A'
        n2l n = chr (n + ord 'A')

main :: IO ()
main = interact $ unlines . map (caesar 5 . reverse) . lines 
