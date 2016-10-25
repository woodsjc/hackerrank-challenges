import Data.Function   (on)
import Data.List       (sort, sortBy, isPrefixOf)
import Data.Maybe

main :: IO()
main = do
	args <- getContents
	mapM_ print $ solve $ lines args


parse_crosswords :: String -> [String]
parse_crosswords str = reverse. sortBy (compare `on` length) $ splitOn ";" str


--choosing the right crossword needs to be at a high level so it will remove from the potential list
--this should send back list of lengths & position [len, start pos, end pos]
parse_str :: Int -> String -> [Int]
parse_str n [] = []
parse_str n (x:xs)  
           | x == '-'  = (\z -> if z > 1
                            then [z,n,n+z] ++ parse_str (n+z) (pop z (x:xs))
                            else parse_str (n+z) (pop z (x:xs))
                        ) $ len_section (x:xs)
           | otherwise = parse_str (n+1) xs 


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
solve args = solve' ((parse_across args) ++ [[0]])
                    ((parse_down args) ++ [[0]])
                    (parse_crosswords $ last args)
                    (init args)


--start up a recursive instance for each instance of across or down equal in length to crossword answer
--if fill returns maybe need to backtrack or swap order of crosswords 
--maybe always try to fit last of same length, but without the infinite loop
--not sure how to preserve list when there isn't a hit in fill may need another parameter 
--or just rerun parse on the updated ans
--need to get the x&y position of the puzzle into fill (not currently happening)
solve' :: [[Int]] -> [[Int]] -> [String] -> [String] -> [String]
solve' across down [] ans = ans
solve' []     []   cw ans = ans
solve' across down cw ans 
     | (\[z,_,_] -> z) (head across) == length (head cw) = (\x -> case x of 
                                            Just x  -> solve' (tail across) down (tail cw) x
                                            Nothing -> solve' (tail across) down cw        ans
                                            ) $ fill_cw (head cw) (z, \(a1,a2,a3) -> (head across),(last across)) ans
     | (\[z,_,_] -> z) (head down  ) == length (head cw) = (\x -> case x of
                                            Just x  -> solve' across (tail down) (tail cw) x
                                            Nothing -> solve' (tail across) down cw        ans
                                            ) $ fill_cw (head cw) (last down) ans
     | otherwise                                         = ans


solve'' []     []   cw ans a_pos d_pos = ans
solve'' across down [] ans a_pos d_pos = ans
solve'' across down cw ans a_pos d_pos = 



--fill should use info from match length
--z > 0 is across and z <= 0 is down
--need to recursively fill based on highest blanks in puzzle
--not 100% sure how to deal with multiple matches
fill_cw :: String -> [Int] -> [String] -> Maybe [String]
fill_cw cw [x,y,z] puzzle 
      | z >  0 = (\p -> if can_fill p y cw
                          then Just $ insert_list (fill p y cw) x puzzle
                          else Nothing
                   ) $ puzzle !! x
      | z <= 0 = (\p -> if can_fill p y cw
                          then Just $ insert_list (fill p y cw) x (puzzle) --this needs to be sideways for down
                          else Nothing
                   ) $ map (!! x) puzzle


insert_list :: String -> Int -> [String] -> [String]
insert_list str n puzzle = (\(ys,zs) -> 
                            if length zs > 0
                                then ys ++ [str] ++ tail zs
                                else init ys ++ [str]) $ splitAt n puzzle 


can_fill :: String -> Int -> String -> Bool
can_fill str n cw | n > length str && length cw > 0 = False
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