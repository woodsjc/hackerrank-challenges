import Data.List (elemIndex)
import Data.Maybe (fromJust)

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

displayPathtoPrincess :: Int -> [String] -> [String]
displayPathtoPrincess i grid = convert (findChar 0 'm' grid) (findChar 0 'p' grid)

convert (mx,my) (px,py) = s (mx-px, my-py)
    where s (sx,sy)
           | sx > 0 = "LEFT"  : s (sx-1,sy)
           | sx < 0 = "RIGHT" : s (sx+1,sy)
           | sy > 0 = "UP"    : s (sx,sy-1)
           | sy < 0 = "DOWN"  : s (sx,sy+1)
           | otherwise = []


findChar :: Int -> Char -> [String] -> (Int,Int)
findChar n _ [] = (0,0)
findChar n c grid = (\x -> if x == Nothing 
                            then findChar (n+1) c (tail grid) 
                            else (n,fromJust x)) 
                    $ elemIndex c (head grid)

main :: IO()
main = do
    n <- getLine
    let i = read n
    grid <- getList i
    mapM_ putStrLn $ displayPathtoPrincess i grid

