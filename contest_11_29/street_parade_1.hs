import Data.List (sort)

{-input:
  length of list of boundaries
  list of boundaries between segments
  m, h_min, h_max
    m = total walk distance at 1 segment per time unit
    h_min = optimal minimum length per segment
    h_max = optimal maximum length per segment
  solution:
    sliding window for beginning - walk dist to end + 1
    since there is a guaranteed solution can just find first solution
-}
main :: IO()
main = do
    contents <- fmap (map (map read. words). lines) getContents
    let seg = head $ drop 1 contents
        m = head $ last contents
        h_min = head $ drop 1 $ last contents
        h_max = last $ last contents
    print $ solve seg m h_min h_max

solve :: [Int] -> Int -> Int -> Int -> Int
solve seg m h_min h_max = solve' (find_range m s) s m h_min h_max 
  where s = sort seg

find_range m seg = [(head seg - m)..(last seg + 1)]

solve' :: [Int] -> [Int] -> Int -> Int -> Int -> Int
solve' [] _ _ _ _ = 0
solve' (r:rs) seg m h_min h_max = if isOptimal r m h_min h_max seg 
                                     then r
                                     else solve' rs seg m h_min h_max

isOptimal _ _ _ _ [] = True
isOptimal start m h_min h_max (end:ss) 
        | m <= 0 = True
        | d > m = if m >= h_min && m <= h_max then True else False
        | d >= h_min && d <= h_max = isOptimal end (m-d) h_min h_max ss 
        | otherwise = False
  where d = end - start
