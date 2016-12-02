import Data.List (sort)

main :: IO()
main = do
    contents <- fmap (map read. words) getLine
    let m = head contents
        n = last contents
    print $ solve $ [m..n]

solve :: [Int] -> Int
solve = count_primes_within_2 . getPrimes 

count_primes_within_2 [] = 0
count_primes_within_2 (x:y:xs) = (if y - x == 2 then 1 else 0) 
                               + count_primes_within_2 (y:xs)
count_primes_within_2 _ = 0                               

getPrimes x = filter (prime_check 2) x

prime_check n d
          | gt_sqrt n d = True
          | d `mod` n == 0 = False
          | otherwise = prime_check (n+1) d
  where gt_sqrt a b = fromIntegral a > sqrt (fromIntegral d)
