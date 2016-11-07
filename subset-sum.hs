import Data.List (sort)

main :: IO()
main = do
    contents <- fmap lines getContents
    let nums = map read. words $ contents !! 1
        s = map read $ drop 3 contents
    mapM_ print $ map (\x -> solve x (convert 0 $ reverse $ sort nums)) s

solve :: Int -> [Int] -> Int
solve s nums
    | s > last nums = -1
    | otherwise = min_sub s 1 nums

min_sub _ _ [] = -1
min_sub limit n (x:xs)
      | limit <= x = n
      | otherwise  = min_sub limit (n+1) xs

convert _ [] = []
convert prev (n:ns) = prev + n : convert (prev+n) ns


