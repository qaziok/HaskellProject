{- Dla danej liczby n należy znaleźć liczbę jej dzielników, mniejszych od n. Uwaga: złożoność
programu powinna wynosić O(p + k), gdzie p – największy dzielnik n mniejszy od sqrt(n), k – liczba
dzielników n. -}

module Main where

integerSqrt :: Integer -> Integer
integerSqrt = floor . sqrt . fromIntegral

addThis :: Integer -> Integer -> Integer
addThis n x = if x ^ 2 /= n then 2 else 1

factors :: Integer -> Integer
factors1 n = sum [addThis n x | x <- [1..integerSqrt n], mod n x == 0] - 1

main :: IO ()
main = do print "Enter the number:"
          n <- readLn 
          print (factors n)