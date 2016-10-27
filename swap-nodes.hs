import Data.List


data Tree = Empty | Node Tree Int Tree deriving (Show, Eq)


--input: 
--first line length of each tree
--middle 2*N elements of tree
--after tree is the number of swaps
--following the number of swaps is the depth at which to swap
--understood 1 at start of tree (not mentioned anywhere)
main :: IO()
main = do
	_ <- getLine
	tmp <- fmap ((map words). lines) getContents
  	let 
  		nodes = map read $ concat $ head $ split_args ([["1"]] ++ tmp)
		swaps = tail $ map read $ concat $ head $ tail $ split_args tmp
	mapM_ putStrLn $ map (unwords. map show) $ run_swaps swaps $ buildTree nodes
	

split_args :: [[String]] -> [[[String]]]
split_args args = (\x -> [take (length args - length x) args] ++ [x] ) $ reverse $ s args
	where
		s [] = []
		s y
		 | length (last y) < 2 = (last y) : s (init y)
		 | otherwise           = []


run_swaps :: [Int] -> Tree -> [[Int]]
run_swaps []     _    = [[]]
run_swaps [s]    tree = [inorder $ swap_DF tree s 1]
run_swaps (s:ss) tree = (\x -> [inorder x] ++ run_swaps ss x)
					$ swap_DF tree s 1


--detect and padd empty places so build tree will work
--take 15 [2^j-1 | j <- [1..]]
convert_args :: [Int] -> [Int]
convert_args nodes = pad nodes 0
	where
		pad nodes cur 
		   | cur > node_max (length nodes) = nodes
		   | nodes !! cur == -1            = pad (take ((cur+1)*2-1) nodes 
		   	                                      ++ [-1,-1] 
		   	                                      ++ drop ((cur+1)*2-1) nodes)
		   	                                     (cur+1)
		   | otherwise                     = pad nodes (cur+1)
		node_max n_len = if n_len > 2^14
			then 0 
			else fromIntegral (2^(floor $ logBase 2 (fromIntegral n_len))-1)


--nodes come in level order as list
--[1,2,3,4] becomes r rl rr rll (and would have a lot of -1) [1,2,3,4,-1,-1,-1]
--tree not actually symmetric, so need a new way to build
buildTree []    = Empty
buildTree nodes = if head nodes == -1 
	then Empty 
	else Node (buildTree (separateLR left  (tail nodes)))
              (head nodes)
              (buildTree (separateLR right (tail nodes)))
   where
   	left  = isLeft (length nodes - 1) 0
   	right = map not left 


separateLR :: [Bool] -> [a] -> [a]
separateLR cond []    = []
separateLR []   nodes = []
separateLR cond nodes = (if (head cond) then [head nodes] else [])
                        ++ separateLR (tail cond) (tail nodes)


isLeft :: Int -> Int -> [Bool]
isLeft len power
  | len <= (2^power) = take len $ replicate (2^power) True ++ replicate (2^power) False
  | otherwise        = replicate (2^power) True 
  					   ++ replicate (2^power) False 
  					   ++ isLeft (len - 2^(power+1)) (power + 1)


swap (Node left value right) = Node right value left

get_left  (Node left _     _    ) = left
get_right (Node _    _     right) = right
get_value (Node _    value _    ) = value

get_children x
   | x == Empty                                  = []
   | get_left x == Empty && get_right x == Empty = []
   | get_left x == Empty && get_right x /= Empty = [get_right x] 
   | get_left x /= Empty && get_right x == Empty = [get_left x]
   | otherwise                                   = [get_left x] ++ [get_right x]


traverseBF :: Tree -> [Int]
traverseBF tree = tbf [tree]
	where
		tbf :: [Tree] -> [Int]
		tbf []    = []
		tbf level = map get_value level ++ (tbf $ concat $ map get_children level)


--swaps also occur at multiples of swap depth
swap_DF :: Tree -> Int -> Int -> Tree 
swap_DF cur depth c_depth
	  | cur == Empty             = Empty
      | c_depth `mod` depth == 0 = Node (swap_DF (get_right cur) depth (c_depth + 1))
      									(get_value cur)
      									(swap_DF (get_left cur) depth (c_depth + 1))
      | otherwise                = Node (swap_DF (get_left cur) depth (c_depth + 1))
                                        (get_value cur)
                                        (swap_DF (get_right cur) depth (c_depth + 1))
--      | c_depth <  depth         = Node (swap_DF (get_left cur) depth (c_depth + 1))
--                                (get_value cur)
--                                (swap_DF (get_right cur) depth (c_depth + 1))
--      | otherwise	             = Empty


inorder :: Tree -> [Int]
inorder Empty        = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r