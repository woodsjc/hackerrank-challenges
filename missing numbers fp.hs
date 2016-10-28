import Data.List (elemIndex, sort, group)


main :: IO()
main = do
    [_,a,_,b] <- fmap ((map words). lines) getContents
    putStrLn $ unwords $ map (show. head) $ group $ sort $ step (map read a) (map read b) 


step :: [Int] -> [Int] -> [Int]
step []     b   = b
step (a:as) b
   | elemIndex a b /= Nothing = step as 
                                     ((\(Just x) -> take x b ++ drop (x+1) b) 
                                        $ elemIndex a b)
   | otherwise                = a : step as b
