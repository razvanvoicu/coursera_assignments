import Control.Monad
import Data.List

main :: IO ()
main = do
	input1 <- getLine
	input2 <- getLine
	print $ length $ filter (\(x,y) -> x /= y) $ zip input1 input2

