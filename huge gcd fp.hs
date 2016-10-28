main :: IO()
main = do
    contents <- fmap (map words. lines) getContents
    let
        a = foldl (*) 1 (map read $ contents !! 1)
        b = foldl (*) 1 (map read $ last contents)
    print $ (gcd a b) `mod` (10^9+7)


