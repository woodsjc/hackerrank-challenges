import System.IO (getLine)
import Control.Monad


main :: IO()
main = do
  num_samples <- getLine
  replicateM_ (read num_samples) read_input


maxsub :: [Int] -> Int -> [Int]
maxsub x y
    | last(x) + y > head(x) = if last(x) + y > 0
                              then [last(x) + y, last(x) + y]
                              else [last(x) + y, 0]
    | otherwise             = if last(x) + y > 0
                              then [head(x), last(x) + y]
                              else [head(x), 0]


kadanes :: [Int] -> Int
kadanes xs = head $ foldl maxsub [0,0] xs


parse_int_array :: String -> [Int]
parse_int_array tmp_array = map read $ words tmp_array


read_input = do
    n  <- getLine
    tmp_array <- getLine
    let xs = parse_int_array tmp_array
    if length (filter (>0) xs) == 0
       then do
      putStr   $ show $ foldl max (-9999999999) xs
      putStr   " "
      putStrLn $ show $ foldl max (-9999999999) xs
       else do
      putStr   $ show $ kadanes xs
      putStr   " "
      putStrLn $ show $ sum $ filter (>0) xs

