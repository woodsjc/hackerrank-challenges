import System.IO (getLine)
import Control.Monad


main :: IO()
main = do
  num_samples <- getLine
  replicateM_ (read num_samples) read_input


mod_7 :: Int -> String
mod_7 n 
    | n `mod` 7 == 1 = "Second" 
    | n `mod` 7 == 0 = "Second"
    | otherwise      = "First"


read_input = do
    n  <- getLine
    putStrLn   $ mod_7 $ read n

