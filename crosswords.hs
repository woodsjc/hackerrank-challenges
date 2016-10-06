import Data.Function   (on)
import Data.List       (sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe

main :: IO()
main = do
	args <- getContents
	mapM_ print $ solve $ lines args


parse_crosswords :: String -> [String]
parse_crosswords str = reverse. sortBy (compare `on` length) $ splitOn ";" str


--choosing the right crossword needs to be at a high level so it will remove from the potential list
--this should send back list of lengths & position [len, row, col]
parse_str :: Int -> String -> [Int]
parse_str n [] = []
parse_str n (x:xs)  
           | x == '-'  = (\z -> [z,n,n+z] ++ parse_str (n+z) (pop z (x:xs))) $ len_section (x:xs)
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


parse_across args = reverse $ sort $ map (parse_str 0) $ init args
parse_down   args = reverse $ sort $ map (parse_str 0) $ 
               map (\x -> map (!! x) $ init args) [0..((length $ init args)-1)]
parse args   = (\x -> [(parse_across x)
                      ,(parse_down x)
                      ]
                ) args


--not sure what type will end up being
--likely need to rewrite parsing to include positions
solve :: [String] -> [String]
solve args = solve' (parse args) (parse_crosswords last args) (init args)

--start up a recursive instance for each instance of across or down equal in length to crossword answer
solve' [across,down] [] ans = ans
solve' [across,down] cw ans 
     | length (head across) == length (head cw) = solve' 
                                                    [(tail across),down] (tail cw) 
                                                    (fill_cw (head cw) (head across) ans)
     | length (head down  ) == length (head cw) = solve' 
                                                    [across,(tail down)] (tail cw) 
                                                    (fill_cw (head cw) (head down) ans)
     

--fill should use info from match length
--z > 0 is across and z <= 0 is down
--need to recursively fill based on highest blanks in puzzle
--not 100% sure how to deal with multiple matches
fill_cw :: String -> (Int,Int,Int) -> [String] -> Maybe [String]
fill_cw cw (x,y,z) puzzle 
      | z >  0 = (\p -> if can_fill p y cw
                          then Just $ insert_list (fill p y cw) x puzzle
                          else Nothing
                   ) $ puzzle !! x
      | z <= 0 = (\p -> if can_fill p y cw
                          then Just $ insert_list (fill p y cw) x (puzzle) --this needs to be sideways for down
                          else Nothing
                   ) $ map (!! x) puzzle
      where
        

insert_list :: String -> Int -> [String] -> [String]
insert_list str n puzzle = (\(ys,zs) -> 
                            if length zs > 0
                                then ys ++ str ++ tail zs 
                                else init ys ++ str) $ splitAt n puzzle 


can_fill :: String -> Int -> String -> Bool
can_fill str n cw | n > length str && length cw > 0 = False
can_fill str n [] = True
can_fill str n cw = if str !! n == '-' || str !! n == head cw 
                      then can_fill str (n+1) (tail cw)
                      else False


fill :: String -> Int -> String -> String
fill str n cw = take n str ++ cw ++ take (n-length cw) (reverse str)

