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
    where range = take 3 $ drop 1 $ reverse prices

--cap at 1/3 current money per stock
--need to know buy or sell status of all items before assigning variable weights
--current weighting is pretty bad
actions :: Double -> [(Double, Index)] -> [String]
actions _ [] = []
actions m (i@(r,index):is) = (\buy_price -> 
    [buy_or_sell buy_price i] ++ actions (m-buy_price) is
    ) $ max_buy m (get_current_price index)
      * (if length is > 0 && fst (head is) < 0.5 then 0.66 else 1.0)

--need a way to figure out how many to buy
--take good deals & map onto avaliable (equally)
buy_or_sell :: Double -> (Double, Index) -> String
buy_or_sell money (rating, index@(Index name owned prices)) 
   | rating >= 1.15 && owned > 0 = sell
   | rating <= 0.5 && money >= last prices = buy
   | otherwise = ""
   where buy = name ++ " BUY " ++ (show (floor $ money / (last prices)))
         sell = name ++ " SELL " ++ (show owned)

max_buy :: Double -> Double -> Double
max_buy money price = if money > price 
    then price + max_buy (money-price) price 
    else 0.0

get_prior_rating :: Index -> Double
get_prior_rating (Index name owned prices) = (last prices) / (last $ tail prices)

--only looks to at most 5 prior prices
downward_indicator :: Index -> Bool
downward_indicator (Index name owned prices) = dec 1 prices
  where dec n []     = False
        dec n prices
          | n > 5 = True
          | length prices < 2 = True
          | otherwise = if last prices > (last $ init prices) 
                           then False 
                           else dec (n+1) (init prices)

