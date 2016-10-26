main :: IO()
main = do
	args <- fmap lines getContents
	putStrLn $ (\x -> (show $ length x) ++ " " ++ x
					++ "\n" ++ (show $ length (drop (length x) (head args))) ++ " " ++ (drop (length x) (head args))
					++ "\n" ++ (show $ length (drop (length x) (head $ tail args))) ++ " " ++ (drop (length x) (head $ tail args))
					) 
				 $ prefix_comp (head args) (head $ tail args)


prefix_comp :: String -> String -> String
prefix_comp [] _  = []
prefix_comp _  [] = []
prefix_comp x  y = if (head x) == (head y) 
	then [head(x)] ++ prefix_comp (tail x) (tail y) 
	else []