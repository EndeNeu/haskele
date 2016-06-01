module Double where

-- function definition
doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

-- for comprehension
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

-- index of array
getFromArray arr index = arr !! index

-- is empty list
isListEmpty list = null list

contains list e = e `elem` list

doubleList list = [x * 2 | x <- list]

length' xs = sum [1 | _ <- xs]

-- for with filter
doubleWithCondlist = [x*2 | x <- [1..10], x*2 >= 12]

-- *Main> doubleDynamic [1,2,3] (*2) (>2)
--   [6]
-- *Main> doubleDynamic [1,2,3,4,5] (*2) (>2)
--   [6,8,10]
-- *Main> doubleDynamic [1,2,3,4,5] (\x -> x*2) (\x -> x>2)
--   [6,8,10]
doubleDynamic :: [Int] -> (Int -> Int) -> (Int -> Bool) -> [Int]
doubleDynamic list op filter = [op x | x <- list, filter x]

-- recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- . is used for function composition
sum (replicate 5 (max 6.7 8.9))
(sum . replicate 5 . max 6.7) 8.9 -- where replicate takes 2 parameters, same for max.import