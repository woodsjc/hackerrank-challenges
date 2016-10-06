import Data.List


main :: IO()
main = do
	args <- getContents
	mapM_ putStrLn $ map (unwords. rotate) $ tail $ lines args


rotate :: String -> [String]
rotate str = rotate' (length str) str


rotate' 1 str = [(tail str ++ [head str])]
rotate' n str = (tail str ++ [head str]) : rotate' (n-1) (tail str ++ [head str])