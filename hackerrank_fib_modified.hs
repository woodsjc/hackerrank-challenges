import System.Environment (getArgs)
import System.IO (readLn, getLine)

main :: IO()
main = do
--  args <- getArgs
  args <- getLine
  let xs = map read $ words args
  let x = head xs
  let y = head $ tail xs
  let z = last xs
  putStrLn . show $ mod_fib x y (z-1)

mod_fib :: Integer -> Integer -> Integer -> Integer
mod_fib x y 0 = x
mod_fib x y 1 = y
mod_fib x y z = mod_fib x y (z-2) + (mod_fib x y (z-1)) ^ 2

toInt :: String -> Int
toInt x = read x

