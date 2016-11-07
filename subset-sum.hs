import Data.List (sort, sortBy)
import Data.Function (on)

main :: IO()
main = do
    contents <- fmap lines getContents
    let nums = map read. words $ contents !! 1
        s = map read $ drop 3 contents
    --mapM_ print $ map (\x -> solve x (convert 0 $ reverse $ sort nums)) s
    mapM_ print $ solve' s $ convert 0 $ reverse $ sort nums

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

solve' :: [Int] -> [Int] -> [Int]
solve' s nums = map (\[a,b,c] -> c) 
                    $ sortBy (compare `on` (\[a,b,c] -> b))
                        $ min_sub' (sort $ pad 0 s) 1 nums

min_sub' :: [[Int]] -> Int -> [Int] -> [[Int]]
min_sub' [] _ _ = [[]]
min_sub' s _ [] = map (\[a,b] -> [a,b,-1]) s
min_sub' (s@[a,b]:ss) i (n:ns)
      | a <= n    = [a,b,i] : min_sub' ss i (n:ns)
      | otherwise = min_sub' (s:ss) (i+1) (ns)


pad _ [] = []
pad n (s:ss)  = [s,n] : pad (n+1) ss
