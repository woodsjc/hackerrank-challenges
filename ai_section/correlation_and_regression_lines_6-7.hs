correlate :: [Double] -> [Double] -> Double
correlate x y = covariance x y / ((stdev x) * (stdev y))


average x = sum x / (fromIntegral $ length x)
stdev x = sqrt $ var x
var x = e (average x) x / (fromIntegral $ length x)
    where e _ [] = 0
          e u (x:xs) = (x - u)^2 + e u xs

covariance x y = xy_avg - x_avg * y_avg
    where x_avg = average x
          y_avg = average y
          xy_avg = average $ zipWith (*) x y

main = do putStrLn "lul"