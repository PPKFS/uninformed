module Main (main) where

import Uninformed (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
