import Data.List (group, sort, nub)
import Text.Printf (printf)

main :: IO()
main = do
    content <- fmap lines getContents
    let x = map read $ words $ last content
    printf "%.1f\n" $ mean x
    printf "%.1f\n" $ median x
    putStrLn $ show $ mode x
    printf "%.1f\n" $ stdev x
    (\(a,b) -> printf "%.1f %.1f\n" a b) $ confidence_95int x

mean :: Num a => [Int] -> Double    
mean x = (fromIntegral $ sum x) / (fromIntegral $ length x)

mode :: [Int] -> Int
mode x = mode' $ map snd $ (\a -> (\b -> filter ((==b).fst) a) $ fst $ last $ sort a) 
            -- $ sort $ map (\z -> (length z, head z)) $ group $ sort x
            $ map (\y -> (length $ filter (==y) x, y)) $ nub x


mode' [] = 10^10
mode' (x:xs) = min x $ mode' xs

stdev :: [Int] -> Double    
stdev x = sqrt $ var $ map fromIntegral x

var x = e (mean x) x / (fromIntegral $ length x)
    where e _ [] = 0.0
          e u (x:xs) = (fromIntegral x - u)^2 + e u xs

median :: [Int] -> Double    
median x = median' $ sort x

median' [] = 0.0
median' x
     | length x == 1 = fromIntegral $ head x
     | length x == 2 = mean x
     | otherwise     = median' $ init $ tail x

confidence_95int ::  [Int] -> (Double, Double)
confidence_95int x = (a - 1.96*s/n, a + 1.96*s/n)
    where a = mean x
          s = stdev x
          n = sqrt $ fromIntegral $ length x