{-  Dla trójkąta podanego przy pomocy trzech różnych punktów a, b, c na płaszczyźnie kartezjańskiej,
dla której -1000 ≤ x, y ≤ 1000. Podać odpowiedź czy zawiera środek układu współrzędnych
(x,y)=(0,0). Przykładowo trójkąt: a=(-100,100), b=(-100, -100), c=(100, 100) zawiera a trójkąt a=(-
100,100), b=(-100, -99), c=(100, 100) nie zawiera punktu (0,0). -}

{-insideCheck :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
insideCheck vertexA vertexB vertexC = -}

module Main where

-- 
tupleToList :: [(a,a,a)] -> [a]
tupleToList ((a,b,c):xs) = a : b : c : tupleToList xs
tupleToList _          = []

getThird :: (Float, Float, Float) -> Float
getThird list = last (tupleToList list)

linearFunction :: (Float, Float) -> (Float, Float) -> (Float, Float, Float)
linearFunction vertexX vertexY = (slope vertexX vertexY, -1, intercept vertexX vertexY (slope vertexX vertexY))

slope :: (Float, Float) -> (Float, Float) -> Float
slope x y = (snd y - snd x) / (fst y - fst x)

intercept :: (Float, Float) -> (Float, Float) -> Float -> Float
intercept x y slope = snd x - slope * fst x

check :: (Float, Float, Float) -> (Float, Float) -> Float
check line point = (fst line * fst point + snd line * snd point + getThird line) * getThird line

ifInside :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
ifInside a b c = check (linearFunction a b) c > 0 && check (linearFunction a c) b > 0 && check (linearFunction b c) a > 0

main :: IO()
main = do print "shit"