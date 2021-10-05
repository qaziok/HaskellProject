{- Dla danej liczby n należy znaleźć liczbę jej dzielników, mniejszych od n. Uwaga: złożoność
programu powinna wynosić O(p + k), gdzie p – największy dzielnik n mniejszy od sqrt(n), k – liczba
dzielników n. -}

module Main where

integerSqrt :: Integer -> Integer
integerSqrt = floor . sqrt . fromIntegral

complement :: Integer -> Integer -> [Integer]
complement n x = if x ^ 2 /= n then [n `div` x, x] else [x]

factors :: Integer -> Int 
factors n = length (concat [complement n x | x <- [1..integerSqrt n], mod n x == 0])

main :: IO ()
main = do print "Enter the number:"
          n <- readLn 
          print (factors n)