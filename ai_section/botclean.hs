import Data.List (elemIndex, group, sort)
import Data.Maybe (fromJust)

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

next_move :: String -> [String] -> String
next_move pos board = grabMost $ convert (get_pos pos) (findChar 0 'd' board)

grabMost z = snd $ last $ sort $ map (\x -> (length x, head x)) $ group z

convert (mx,my) (px,py) = s (mx-px, my-py)
    where s (sx,sy)
           | sy > 0 = "LEFT"  : s (sx,sy-1)
           | sy < 0 = "RIGHT" : s (sx,sy+1)
           | sx > 0 = "UP"    : s (sx-1,sy)
           | sx < 0 = "DOWN"  : s (sx+1,sy)
           | otherwise = "CLEAN" : []

get_pos = (\[x,y] -> (read x,read y)) . words

findChar :: Int -> Char -> [String] -> (Int,Int)
findChar n _ [] = (0,0)
findChar n c grid = (\x -> if x == Nothing 
                            then findChar (n+1) c (tail grid) 
                            else (n,fromJust x)) 
                    $ elemIndex c (head grid)

main :: IO()
main = do
    -- Take input
    pos <- getLine
    board <- getList 5
    -- Do I/O stuff here.
    putStrLn $ next_move pos board
