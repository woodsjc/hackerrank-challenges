main :: IO()
main = do
	args <- fmap lines getContents
	mapM_ putStrLn $ Prelude.map str_reduce args


str_reduce :: String -> String
str_reduce x = seen [] x
	where 
		seen y []     = []
		seen y (x:xs)
		   | x `elem` y = seen y xs
		   | otherwise  = x : seen (x:y) xs 