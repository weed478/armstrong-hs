module Main where

import Armstrong

main :: IO ()
main = mapM_ print armstrongNumbers
