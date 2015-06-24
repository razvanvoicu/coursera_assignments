import Control.Monad
import Data.List

main :: IO ()
main = do
	input <- getLine
	pattern <- getLine
	let subs = map (take $ length pattern) $ tails input
	print $ length $ filter (== pattern) subs

