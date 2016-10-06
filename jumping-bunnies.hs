main :: IO()
main = do
	args <- getContents
	mapM_ print $ map jump $ tail $ lines args


jump :: String -> Int
jump str = foldl lcm 1 $ map read $ words str
