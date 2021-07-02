module Main (main) where

import Intransitivity (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
