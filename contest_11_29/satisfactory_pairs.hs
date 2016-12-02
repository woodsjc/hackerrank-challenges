{-
    https://en.wikipedia.org/wiki/Diophantine_equation
    http://mathoverflow.net/questions/27884/non-negative-integer-solutions-of-a-single-linear-diophantine-equation
    http://mathforum.org/library/drmath/view/51595.html
    http://stackoverflow.com/questions/4917003/whats-algorithm-used-to-solve-linear-diophantine-equation-ax-by-c

-}

main :: IO()
main = do
    n <- fmap read getLine
    print $ solve n

solve n = sum $ map (\a -> count_solutions a (get_pairs a n) n) [1..(n-2)]

get_pairs a n = [(a+1)..(n-1)]

has_solution a b n g = if n `mod` g == 0 then True else False


count_solutions _ [] _ = 0
count_solutions a (b:bs) n
              | a + b > n = 0
              | otherwise = (if has_solution a b n g then 1 else 0)
                            + count_solutions a bs n
    where g = gcd a b