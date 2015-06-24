import Control.Monad
import Data.List

main :: IO ()
main = do
	pattern <- getLine
	input <- getLine
	let subs = map (take $ length pattern) $ tails input
	let pats = filter ((== pattern) . snd) $ zip [0..length input - length pattern - 1] subs
	putStrLn $ intercalate " " $ map (show . fst) pats

