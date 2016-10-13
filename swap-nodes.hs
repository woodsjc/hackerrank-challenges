import Data.List


data Tree = Empty | Node Tree Int Tree deriving (Show, Eq)


--input: 
--first line length of each tree
--middle 2*N elements of tree
--after tree is the number of swaps
--following the number of swaps is the depth at which to swap
main :: IO()
main = do
	nodes <- fmap read getLine
	tmp <- getContents
	let args = map read $ concatMap words $ lines tmp 
	mapM_ putStrLn $ map (unwords. map show)
		  $ run_swaps (tail $ drop (nodes*2) args) $ buildTree $ take (nodes*2) args
	

run_swaps :: [Int] -> Tree -> [[Int]]
run_swaps [] _    = [[]]
run_swaps xs tree = (\x -> [traverseBF x] ++ run_swaps (tail xs) x) $ swap_DF tree (head xs) 1


--nodes come in level order as list
--[1,2,3,4] becomes r rl rr rll (and would have a lot of -1) [1,2,3,4,-1,-1,-1]
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
  | len < (2^power) = take len $ replicate (2^power) True ++ replicate (2^power) False
  | otherwise       = replicate (2^power) True 
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



swap_DF :: Tree -> Int -> Int -> Tree 
swap_DF cur depth c_depth
	  | cur == Empty     = Empty
      | c_depth == depth = swap cur
      | c_depth <  depth = Node (swap_DF (get_left cur) depth (c_depth + 1))
                                (get_value cur)
                                (swap_DF (get_right cur) depth (c_depth + 1))
      | otherwise	     = Empty


inorder :: Tree -> [Int]
inorder Empty        = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r