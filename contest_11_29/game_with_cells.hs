main :: IO()
main = do
    contents <- fmap words getLine
    let c = map read contents
        n = head c
        m = last c
    putStrLn $ show $ solve n m

solve :: Int -> Int -> Int
solve n m
    | n `mod` 2 == 1 && m `mod` 2 == 1 = (n+1) * (m+1) `div` 4
    | n `mod` 2 == 1 && m `mod` 2 == 0 = (n+1) * m `div` 4
    | n `mod` 2 == 0 && m `mod` 2 == 1 = n * (m+1) `div` 4
    | n `mod` 2 == 0 && m `mod` 2 == 0 = n * m `div` 4