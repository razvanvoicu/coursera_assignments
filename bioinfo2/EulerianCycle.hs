{-# LANGUAGE TupleSections #-}
--module EulerianCycle where

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
                    (Just a, Just b) | a < b -> True
                    (Nothing, Just _) -> True
                    _ -> False

unbalancedIn :: Map.Map String Int -> Map.Map String Int -> [String]
unbalancedIn inDeg outDeg = filter f $ Map.keys inDeg
    where f x = case (Map.lookup x inDeg, Map.lookup x outDeg) of
                    (Just a, Just b) | a > b -> True
                    (Just _, Nothing) -> True
                    _ -> False

main :: IO ()
main = do
    ls <- lines <$> getContents
    let (g,vs) = foldl `flip` (Map.empty,Set.empty) `flip` ls $
                    \(a,b) s -> let [src',dest'] = splitOn " -> " s
                                    [src] = words src'
                                    dest = splitOn "," dest'
                                in (Map.insert src dest a, Set.unions [b,(Set.fromList $ src : dest)])
    let isStuck (cycle@(h:_),graph) = Map.lookup h graph == Nothing

    let rotate :: ([String],Map.Map String [String]) -> [String]
        rotate (cycle,graph) = let i = fromJust $ findIndex (\v -> Map.lookup v graph /= Nothing) cycle
                                   (p,n:s) = splitAt i cycle
                               in n : s ++ (if head cycle == last cycle then tail p else p) ++ [n]

    let f (cycle,graph) = case graph of
                                m | m == Map.empty -> (cycle,graph)
                                _ -> let c = if isStuck(cycle,graph)
                                             then rotate(cycle,graph)
                                             else cycle
                                         h = head c
                                         d = head $ fromJust $ Map.lookup h graph
                                     in (d:c,removeArc graph h d)
    let uo = unbalancedOut (inDegrees g) (outDegrees g)
        ui = unbalancedIn (inDegrees g) (outDegrees g)
    let iter = ([head $ uo ++ Set.toList vs],g) : map f iter
    let out' = reverse $ fst $ head $ filter (\(_,x) -> x == Map.empty) iter
    let out = case (ui,uo) of
                ([],[]) -> out'
                (i:_,o:_) -> case splitOn [i,o] out' of
                                [p,s] -> o : s ++ tail p ++ [i]
                                _ -> out'
                (_:_,[]) -> error "Has indeg imbalance, but no outdeg imbalance"
                ([],_:_) -> error "Has outdeg imbalance, but no indeg imbalance"
    putStrLn $ intercalate "->" out
