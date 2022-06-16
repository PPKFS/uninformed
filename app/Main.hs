module Main (main) where

import Uninformed (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
