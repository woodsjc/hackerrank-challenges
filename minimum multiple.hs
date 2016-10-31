main :: IO()
main = do
    _ <- getLine
    nums <- fmap (map read. words) getLine
    _ <- getLine
    contents <- fmap lines getContents
    step contents nums


--query :: Num a => a -> a -> [a] -> a
query start finish nums = find_lcm $ take (finish - start + 1) (drop start nums)
    where
        find_lcm xs = (foldl lcm 1 xs) `mod` (10^9+7)


--update :: Num a => a -> a -> [a] -> [a]
update a b nums = take a nums ++ [(nums !! a)*b] ++ drop (a+1) nums


step :: [String] -> [Integer] -> IO()
step []       _                  = return ()
step contents nums
   | head (head contents) == 'Q' = do 
                                    print $ (\[_,a,b] -> query (read a) (read b) nums) $ words (head contents)
                                    step (tail contents) nums
   | head (head contents) == 'U' = step (tail contents) 
                                        $ (\[_,a,b] -> update  (read a) (read b) nums) $ words (head contents)
   | otherwise                   = return ()