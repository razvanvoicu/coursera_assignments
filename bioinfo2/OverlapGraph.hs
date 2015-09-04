{-# LANGUAGE ScopedTypeVariables #-}
module StringSpelledByGenomePath where

import Control.Monad
import Data.Functor
import Data.String
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do
    l <- lines <$> getContents
    let n = length $ l !! 0
    let c = zip (map (take (n-1)) l) (map ( (: []) . drop 1) l)
    let g = Map.fromListWith (++) c :: Map.Map String [String]
    let p = let f a k b = 
                    let h x y = 
                            let q a z = (head k : y, head y : z) : a
                            in foldl q x $ Map.findWithDefault [] y g
                    in foldl h a b
            in Map.foldlWithKey f [] g
    putStrLn $ intercalate "\n" $ sort $ map (\(x,y) -> x ++ " -> " ++ y) p