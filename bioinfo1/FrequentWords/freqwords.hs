import Prelude hiding (fromList)
import Control.Monad
import Data.List
import Data.MultiSet (fromList,toOccurList)

occursOrd (_,x) (_,y) | x < y = GT
                      | x > y = LT
                      | True    = EQ

main :: IO ()
main = do
    input <- getLine
    len <- readLn :: IO Int
    let subs = take (length input - len + 1) $ map (take len) $ tails input
    let freq = toOccurList $ fromList subs
    let sortFreq = sortBy occursOrd freq
    let highest = filter ((== (snd $ head sortFreq)) . snd) sortFreq
    putStrLn $ intercalate " " $ map fst highest
