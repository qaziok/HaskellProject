{- Dla danej liczby n należy znaleźć liczbę jej dzielników, mniejszych od n. Uwaga: złożoność
programu powinna wynosić O(p + k), gdzie p – największy dzielnik n mniejszy od sqrt(n), k – liczba
dzielników n. -}

module Main where
import Data.List ()

integerSqrt :: Integer -> Integer
integerSqrt = floor . sqrt . fromIntegral

factors1 :: Integer -> [Integer]
factors1 n = [x | x <- [1..integerSqrt n], mod n x == 0] -- O(sqrt(n))

factors2 :: Integer -> [Integer] -> [Integer]
factors2 n list = list ++ [n `div` x | x <- reverse list, x ^ 2 /= n] -- O(length (factors1 n))

main :: IO ()
main = do print "Enter the number:"
          n <- readLn 
          print (length(factors2 n (factors1 n)))