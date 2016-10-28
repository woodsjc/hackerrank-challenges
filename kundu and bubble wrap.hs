main :: IO()
main = do
    contents <- fmap (map read. words) getContents
    let
        n = head contents
        m = last contents
    print $ step n m 0


step :: Double -> Double -> Double -> Double
step n m filled
   | filled < n * m = (n * m) / (n*m - filled) + step n m (filled + 1)
   | otherwise      = 0