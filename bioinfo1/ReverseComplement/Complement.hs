{-
   http://rosalind.info/problems/revc/
   Problem

   In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'.
   The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, 
   then taking the complement of each symbol (e.g., the reverse complement of "GTCA" is "TGAC").

   Given: A DNA string s of length at most 1000 bp.

   Return: The reverse complement sc of s.

   Sample Dataset

      AAAACCCGGT

   Sample Output

      ACCGGGTTTT
-}

import System.Environment
import Data.Functor
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    let fname = case args of
                    [] ->  "C:/Users/r/Downloads/rosalind_revc.txt"
                    h:_ -> h
    input <- filter (not . isSpace) <$> readFile fname
    putStrLn $ complementString input

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'G' = 'C'
complement 'C' = 'G'
complement _   = error "Invalid symbol in input file"

complementString :: String -> String
complementString = map complement . reverse
