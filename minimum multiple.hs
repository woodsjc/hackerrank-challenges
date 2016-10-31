main :: IO()
main = do
    n <- fmap read getLine
    --nums <- fmap (map read. words) getLine
    nums <- fmap (map read. words) getLine
    _ <- getLine
    contents <- fmap lines getContents
    --step contents nums
    let queries = map (\x -> (\[[p],q,r] -> (p, read q :: Int, read r :: Int))
                             (words x)) contents
    mapM_ print $ solve n nums queries


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


--segment tree idea from: http://m00nlight.github.io/functional%20programming/2015/04/11/hackerrank-minimum-multiple
--https://en.wikipedia.org/wiki/Segment_tree
data SegTree = Node {
                      val                   :: Integer
                    , left, right           :: Int
                    , leftChild, rightChild :: SegTree
                    } | 
                    Leaf {
                      val         :: Integer
                    , left, right :: Int  
                    }


initSegTree :: Int -> SegTree
initSegTree n = aux 0 (n-1)
    where aux l r 
            | l == r    = Leaf {val= -1, left=l, right=r}
            | otherwise = let mid = (l + r) `div` 2
                          in Node { val= -1, left=l, right=r
                                  , leftChild = aux l mid
                                  , rightChild = aux (succ mid) r
                                  }


s_query :: (Int, Int) -> SegTree -> Integer
s_query range@(l,r) root
      | r < left root                     = 1
      | l > right root                    = 1
      | l <= left root && right root <= r = val root
      | otherwise                         = lcm (s_query range (leftChild root))
                                                (s_query range (rightChild root))


s_update :: Int -> Integer -> SegTree -> SegTree
s_update i newVal root 
       | left root <= i && i <= right root = case root of 
                                                Leaf {} -> root { val = newVal}
                                                _       -> root { val = lcm newVal (val root)
                                                                , leftChild  = lChild
                                                                , rightChild = rChild
                                                                }
       | otherwise                         = root
       where 
        lChild = s_update i newVal $ leftChild root
        rChild = s_update i newVal $ rightChild root


p_queries :: [(Char, Int, Int)] -> SegTree -> [Integer] -> [Integer]
p_queries []              _    acc  = reverse acc
p_queries (('Q', l, r):q) root acc  = let ans = (s_query (l,r) root) `mod` (10^9+7)
                                      in p_queries q root (ans:acc)
p_queries ((_, i, value):q) root acc = let 
                                        oldVal = s_query (i,i) root
                                        newVal = oldVal * (fromIntegral value)
                                        nroot  = s_update i newVal root
                                       in p_queries q nroot acc



solve :: Int -> [Integer] -> [(Char, Int, Int)] -> [Integer]
solve n arr queries = p_queries queries tree []
        where
            tree = foldl (\root (i,v) -> s_update i v root)
                         (initSegTree n)
                         (zip [0..] arr)

