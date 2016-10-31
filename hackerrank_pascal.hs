main :: IO()
main = do
    n <- getLine
    mapM_ putStrLn $ printable $ map pascal [0..((read n)-1)]


printable :: [[Int]] -> [[Char]]
printable p = map (concatMap (\y -> (show y) ++ " ")) p
    
    
pascal :: Int -> [Int]
pascal n = map (\r -> (fac n)`div`((fac r) * (fac (n-r)))) [0..n]


fac :: Int -> Int
fac 0 = 1
fac 1 = 1
fac n = n * fac (n-1)
