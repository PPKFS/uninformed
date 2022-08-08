module Main where


main :: IO ()
main = do
  bs <- readFile "test.txt"
  lex bs