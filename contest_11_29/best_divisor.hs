import Data.List (sort)

main :: IO()
main = do
    contents <- fmap read getLine
    print $ solve $ sort $ get_divs contents

solve :: [Int] -> Int
solve x = get_lowest_eq $ reverse $ sort $ zip (map sum_digits x) x

get_lowest_eq (x@(sum,divisor):xs) = if length xs > 0 && (fst $ head xs) == sum 
                                        then get_lowest_eq xs 
                                        else divisor

sum_digits :: Int -> Int
sum_digits x = x `mod` 10 + if x `div` 10 >= 1 
                               then (sum_digits $ x `div` 10) 
                               else 0


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
