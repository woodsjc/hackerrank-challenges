import Data.Function (on)
import Data.List     (sort, sortBy, isPrefixOf)
import Data.Maybe

main :: IO()
main = do
	args <- fmap lines getContents
	mapM_ putStrLn $ solve args


parse_crosswords :: String -> [String]
parse_crosswords str = reverse. sortBy (compare `on` length) $ splitOn ";" str


--list of lengths & position [len, start pos, end pos]
parse_str :: Int -> String -> [Int]
parse_str n [] = []
parse_str n (x:xs)  
           | x == '-'  = (\z -> if z > 1
                            then [z,n,n+z] ++ parse_str (n+z) (pop z (x:xs))
                            else parse_str (n+z) (pop z (x:xs))
                        ) $ len_section (x:xs)
           | otherwise = parse_str (n+1) xs 


--didn't know about drop when wrote this
pop :: Int -> String -> String
pop _ [] = []
pop 0 ps = ps
pop n ps = pop (n-1) (tail ps)


len_section :: String -> Int
len_section []         = 0
len_section (s:ss) 
           | s == '-'  = 1 + len_section ss
           | otherwise = 0


parse_across :: [String] -> [[Int]]
parse_across args = map (parse_str 0) $ init args


parse_down :: [String] -> [[Int]]
parse_down args = map (parse_str 0) $
                      map (\x -> map (!! x) $ init args) 
                          [0..((length $ init args)-1)]


--not sure what type will end up being
--likely need to rewrite parsing to include positions
solve :: [String] -> [String]
solve args = solve'' (parse_across args)
                     (parse_down args)
                     (parse_crosswords $ last args)
                     [init args]


{-
http://stackoverflow.com/questions/943113/algorithm-to-generate-a-crossword
parse across and down to find open spaces for words
go through word list from the largest to smallest
if a word can't fit due to letter collision try another space
    if nomore spaces wipe prior tree for that level (length of position) and try to fill
    need to keep track of that level and what has been tried
-}
solve'' :: [[Int]] -> [[Int]] -> [String] -> [[String]] -> [String]
solve'' across down []     ans = head ans
solve'' []     []   cw     ans = head ans
solve'' across down (c:cs) ans = solve'' across
                                         down
                                         cs
                                         $ rm_nothing $ concat $ map 
                                           (\x-> map (\y -> fill_cw c x y) ans)
                                           (filter (\r -> length r == 5) (pad across 0)++(pad down 1))


pad xs n = merge (map (\x -> x ++ [n]) xs) [0..(length xs - 1)]


merge:: [[Int]] -> [Int] -> [[Int]]
merge xs     []     = xs
merge []     ys     = [ys]
merge (x:xs) (y:ys) = [x ++ [y]] ++ merge xs ys


rm_nothing []     = []
rm_nothing (x:xs) = case x of 
                    Just z  -> z : rm_nothing xs
                    Nothing -> rm_nothing xs


--l=length, x=start pos, y=end pos, z=across:0 or down:1, r=pos in puzzle
fill_cw :: String -> [Int] -> [String] -> Maybe [String]
fill_cw cw [l,x,y,z,r] puzzle 
      | z == 0 = (\p -> if can_fill p x cw
                          then Just $ insert_list (fill p x cw) r puzzle
                          else Nothing
                   ) $ puzzle !! r
      | z == 1 = (\p -> if can_fill p x cw
                          then Just $ convert $ insert_list (fill p x cw) r (convert puzzle)
                          else Nothing
                   ) $ convert puzzle !! r
fill_cw _ _ _ = Nothing


convert p = map (\x -> map (!! x) p) [0..(length p - 1)] 


insert_list :: String -> Int -> [String] -> [String]
insert_list str n puzzle = (\(ys,zs) -> 
                            if length zs > 0
                                then ys ++ [str] ++ tail zs
                                else init ys ++ [str]) $ splitAt n puzzle 


can_fill :: String -> Int -> String -> Bool
can_fill str n cw | n > length str - 1 && length cw > 0 = False
can_fill str n [] = True
can_fill str n cw = if str !! n == '-' || str !! n == head cw 
                      then can_fill str (n+1) (tail cw)
                      else False


--doesnt work with reverse need to use split & at
fill :: String -> Int -> String -> String
fill str n cw = (\(ys,zs) -> ys ++ cw ++ pop (length cw) zs) $ splitAt n str


--http://stackoverflow.com/questions/5874600/cutting-a-string-into-a-list-in-haskell/5874939#5874939
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn []    _  = error "splitOn: empty delimiter"
splitOn delim xs = loop xs
    where loop [] = [[]]
          loop xs | delim `isPrefixOf` xs = [] : splitOn delim (drop len xs)
          loop (x:xs) = let (y:ys) = splitOn delim xs
                         in (x:y) : ys
          len = length delim