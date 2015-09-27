{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Functor
import Data.String
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

removeArc :: Map.Map String [String] -> String -> String -> Map.Map String [String]
removeArc m src dest = case Map.lookup src m of
    Nothing -> error "Trying to remove non-existent arc"
    Just l -> case delete dest l of
                [] -> Map.delete src m
                x -> Map.insert src x m

outDegrees :: Map.Map String [String] -> Map.Map String Int 
outDegrees = Map.map length                

toPairs :: Map.Map String [String] -> [(String,String)]
toPairs = Map.foldlWithKey f []
    where f a k v = a ++ map (k,) v

flipPairs :: [(String,String)] -> [(String,String)]
flipPairs xs = zip rs ls
    where (ls,rs) = unzip xs

inDegrees :: Map.Map String [String] -> Map.Map String Int
inDegrees = outDegrees . Map.fromListWith (++) . map (\(x,y)->(x,[y])) . flipPairs . toPairs

unbalancedOut :: Map.Map String Int -> Map.Map String Int -> [String]
unbalancedOut inDeg outDeg = filter f $ Map.keys outDeg
    where f x = case (Map.lookup x inDeg, Map.lookup x outDeg) of
                    (Just a, Just b) | a < b || b > 1 -> True
                    (Nothing, Just _) -> True
                    _ -> False

unbalancedIn :: Map.Map String Int -> Map.Map String Int -> [String]
unbalancedIn inDeg outDeg = filter f $ Map.keys inDeg
    where f x = case (Map.lookup x inDeg, Map.lookup x outDeg) of
                    (Just a, Just b) | a > b || a > 1 -> True
                    (Just _, Nothing) -> True
                    _ -> False

dfs :: Int -> Map.Map String [String] -> [String] -> String -> [[String]]
dfs k g stops v = if k > 0 && v `elem` stops
                then [[v]]
                else let nexts = case Map.lookup v g of
                                    Just l -> l
                                    Nothing -> []
                     in  concat $ map (\n -> map (v:) $ dfs (k+1) g stops n) nexts

main :: IO ()
main = do
    kmers <- lines <$> getContents
    let k = length $ kmers !! 0
    let prefixes = map (take (k-1)) kmers
    let suffixes = map ( (: []) . drop 1) kmers
    let c = zip prefixes suffixes
    let g = Map.fromListWith (++) c 
    let vs = Set.fromList $ prefixes ++ concat suffixes
    let uo = unbalancedOut (inDegrees g) (outDegrees g)
        ui = unbalancedIn (inDegrees g) (outDegrees g)
        stops = nub $ ui ++ uo
    let paths = concat $ map (dfs 0 g stops) stops
    let contigs = map (\p -> (map head $ take (length p - 1) p) ++ last p) paths
    putStrLn $ intercalate "\n" contigs
