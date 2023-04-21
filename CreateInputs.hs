module CreateInputs where 

import Data.Set (toList, fromList)

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

first :: Int -> [(Int, Int)]
first size = take size $ map (fix size) $ zip [1,3..] [2, 7 ..]



fix :: Int -> (Int, Int) -> (Int, Int)
fix size (a, b) = 
    (x, y)
    where 
        x = mod ( a*(div b 2) - (a^2) ) (div size 2) 
        y = mod (div x 2 + (mod x b)) (div size  2)

square :: Integer -> [(Integer, Integer)]
square side =  
    [(x,y) | x <- [0..side], y <- [0..side] , x <= side ,   y == 0 ] ++
    [(x,y) | x <- [0..side], y <- [0..side] , x == 0    ,   y <= side ] ++ 
    [(x,y) | x <- [0..side], y <- [0..side] , x <= side ,   y == side ] ++ 
    [(x,y) | x <- [0..side], y <- [0..side] , x == side ,   y <= side ] 


filledSquare :: Integer -> Int -> [(Integer, Integer)]
filledSquare side clear =
    emptyOut 3 $ emptyOut clear theList
    where 
        theList = take (maxBound::Int) [(x,y) | x <- [1..side], y <- [1..side], notElem (x,y) thisSquare, x /= y, notMultiple x y, notMultiple y x]
        thisSquare = square side 


notMultiple :: Integral a => a -> a -> Bool
notMultiple x y 
    | mod x y  == 0 =   False
    |mod x y  == 1  =   False
    |otherwise      =   True

emptyOut :: Int -> [a] -> [a]
emptyOut y (x:xs) = 
    emptyHelper 0 y (x:xs) []
    where 
        emptyHelper _ 0 list retList = retList
        emptyHelper a b [] retList = retList
        emptyHelper num y (x:xs) retList
            |num == y = emptyHelper 0 y (xs) (x : retList)
            |otherwise = emptyHelper (num+1) y (xs) retList

stdFilledSquare :: Integer -> [(Integer, Integer)]
stdFilledSquare = flip filledSquare 11


if' :: Bool -> p -> p -> p
if' True  x _   = x
if' False _ x   = x