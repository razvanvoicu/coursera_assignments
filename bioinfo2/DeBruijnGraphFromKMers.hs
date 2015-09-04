{-# LANGUAGE ScopedTypeVariables #-}
module DeBruijnGraphFromKMers where

import Control.Monad
import Data.Functor
import Data.String
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do
    ls <- lines <$> getContents
    let n = length $ ls !! 0
        m :: Map.Map String [String]
        m = Map.fromListWith (++) $ map (\s -> (take (n-1) s,[drop 1 s])) ls
        o = sort $ Map.foldWithKey (\k v a -> (k ++ " -> " ++ (intercalate "," v)) : a) [] m
    putStrLn $ intercalate "\n" o
