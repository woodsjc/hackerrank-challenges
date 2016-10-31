import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [(area (x_bins l r) a b), (volume (x_bins l r) a b)]


calc_poly :: [Int] -> [Int] -> Double -> Double
calc_poly a b x = sum $ map (\(a,b) -> (fromIntegral a)*x**(fromIntegral b)) $ zip a b


x_bins :: Int -> Int -> [Double]
x_bins l r = [ i*k | i <- [(fromIntegral l)*1000..(fromIntegral r)*1000], k <- [0.001]]


area :: [Double] -> [Int] -> [Int] -> Double
area bins a b = 0.001 * (sum $ map (calc_poly a b) bins)


volume :: [Double] -> [Int] -> [Int] -> Double
volume bins a b = 0.001 * (sum $ map (\x ->  pi*x**2) (map (calc_poly a b) bins))

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
