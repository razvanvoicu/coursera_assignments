import Prelude hiding (fromList)
import Control.Monad
import Data.Functor
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set


occursOrd (_,x) (_,y) | x < y = GT
                      | x > y = LT
                      | True    = EQ

main :: IO ()
main = do
    input <- getLine
    [len,l,t] <- (map read) . words <$> getLine
    let subs = take (length input - len + 1) $ map (take len) $ tails input
    let idxsubs = zip [0..length subs - 1] subs
    let occurs = foldl accum Map.empty idxsubs
    let windows = flip map [0..length input - l - 1] $ \i -> Map.map (filter (\x -> x >= i && x <= i+l)) occurs
    let clumpmaps = map (Map.filter (\x -> length x >= t)) windows
    putStrLn $ intercalate " " $ Set.toList . Set.fromList $ concatMap Map.keys clumpmaps

accum m (i,s) = if Map.member s m then Map.update (Just . (i:)) s m else Map.insert s [i] m 