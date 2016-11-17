import Data.List (sortBy)
import Data.Ord (comparing)

data Index = Index String Int [Double] deriving (Show)

main :: IO()
main = do
    contents <- fmap lines getContents
    let m = read $ pad $ head $ words $ head contents
        index = map words $ tail contents
    putStrLn $ (\x -> show (length $ lines x) ++ "\n" ++ x) 
             $ concatMap (\x -> if length x > 0 then x ++ "\n" else "") 
             $ actions m $ rank_indicies index
     where pad x = if '.' `elem` x then x else x ++ ".0"

read_index :: [String] -> Index
read_index (name:owned:prices) = Index name (read owned) (map read prices)

average :: Fractional a => [a] -> a
average prices = sum prices / (fromIntegral $ length prices)

rank_indicies :: [[String]] -> [(Double, Index)]
rank_indicies index = sortBy (comparing fst) $ map (\x -> (index_rating x, x)) 
                          $ map read_index index

get_current_price :: Index -> Double
get_current_price (Index name owned prices) = last prices
    
index_rating :: Index -> Double
index_rating (Index name owned prices) = last prices / (average $ init prices)

actions :: Double -> [(Double, Index)] -> [String]
actions _ [] = []
actions m (i@(r,index):is) = (\buy_price -> 
    [buy_or_sell buy_price i] ++ actions (m-buy_price) is
    ) $ max_buy m (get_current_price index)

--need a way to figure out how many to buy
--take good deals & map onto avaliable (equally)
buy_or_sell :: Double -> (Double, Index) -> String
buy_or_sell money (rating, index@(Index name owned prices)) 
   | rating >= 1.15 && owned > 0 = name ++ " SELL " ++ (show owned)
   | rating <= 0.5 && money >= last prices = name ++ " BUY " 
                                           ++ (show (floor $ money / (last prices)))
   | otherwise = ""

max_buy :: Double -> Double -> Double
max_buy money price = if money > price 
    then money + max_buy (money-money) price 
    else 0.0