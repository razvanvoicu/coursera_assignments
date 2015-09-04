module StringComposition where

import Data.Functor
import Data.List

main :: IO ()
main = do
	n <- read <$> getLine :: IO Int
	s <- getLine
	putStrLn $ intercalate "\n" $ sort $ filter ((== n) . length) $ map (take n) $ tails s