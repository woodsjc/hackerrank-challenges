main :: IO()
main = do
	args <- getContents
	mapM_ putStrLn $ map compress $ lines args


compress :: String -> String
compress str = compress' str "" 0


compress' :: String -> String -> Int -> String
compress' []  prev count = prev ++ if count > 1 then show count else ""
compress' str _    0     = (compress' (tail str) [(head str)] 1)
compress' str prev count = if prev /= [head str] then 
						   prev ++ 
						   (if count > 1 then show count else "") ++ compress' (tail str) [(head str)] 1
						   else compress' (tail str) [(head str)] (count + 1)
