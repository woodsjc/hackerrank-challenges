import Data.List (intersect)

main :: IO()
main = do
    _ <- getLine
    contents <- map (map read. words). lines <$> getContents
    mapM_ print $ map count_divisors contents

count_divisors :: [Int] -> Int
count_divisors [1,_] = 1
count_divisors [_,1] = 1
count_divisors [a,b] = length $ intersect (get_divs a) (get_divs b)
count_divisors _     = 0

get_divs :: Int -> [Int]
get_divs d = [1] ++ get_divs' 2 d ++ [d]

get_divs' :: Int -> Int -> [Int]
get_divs' n d
        | gt_sqrt n d = []
        | d `mod` n == 0 = if gt_sqrt (other_factor n d) d
                              then [n, other_factor n d] ++ get_divs' (n+1) d
                              else [n] ++ get_divs' (n+1) d
        | otherwise = get_divs' (n+1) d
    where other_factor n d = d `div` n
          gt_sqrt o d = fromIntegral o > sqrt (fromIntegral d)
