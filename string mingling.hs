main :: IO()
main = do
	args <- getContents 
	putStrLn $ (\x -> merge (head x) (last x)) $ lines args


merge :: String -> String -> String
merge xs []          = xs
merge [] ys          = ys
merge (x:xs) (y:ys) = x : y : merge xs ys