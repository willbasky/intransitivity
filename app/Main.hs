module Main (main) where

import Intransitivity (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
