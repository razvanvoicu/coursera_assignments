{-# LANGUAGE ScopedTypeVariables #-}
module DeBruijnGraph where

import Control.Monad
import Data.Functor
import Data.String
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do
    l <- lines <$> getContents
    let n = read $ l !! 0 :: Int
        ls = filter ((n ==) . length) $ map (take n) $ tails $ l !! 1
        m :: Map.Map String [String]
        m = Map.fromListWith (++) $ map (\s -> (take (n-1) s,[drop 1 s])) ls
        o = sort $ Map.foldWithKey (\k v a -> (k ++ " -> " ++ (intercalate "," v)) : a) [] m
    putStrLn $ intercalate "\n" o
