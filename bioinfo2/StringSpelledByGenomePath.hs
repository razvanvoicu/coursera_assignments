module StringSpelledByGenomePath where

import Control.Monad
import Data.Functor
import qualified Data.Map as Map

main :: IO ()
main = do
    l <- lines <$> getContents
    let n = length l 
    putStrLn $ (map head l) ++ (tail $ l !! (n-1))