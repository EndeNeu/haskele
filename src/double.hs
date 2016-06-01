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

-- doubleWithCond2 :: [Int] -> Int -> Int -> Int -> Bool
-- doubleWithCond2 list op predicate = [op | x <- [1..10], predicate]

-- recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- pattern matching
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!" -- case empty list
head' (head:tail) = head -- case head :: tail

-- pattern matching with guards, where allows to define local scoped variables,
-- this can be used all over in the guards.
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- pattern matching with case of, They are useful for pattern matching against something in the middle of an expression
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- calcBmis :: (RealFloat a) => [(a, a)] -> [a]
-- calcBmis xs = [bmi w h | (w, h) <- xs]
--    where bmi weight height = weight / height ^ 2 -- this is a function: bmi :: RealFloar -> RealFloat -> RealFloat

-- for comprehension with let, let is scoped locally only, unlike where
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


-- let defines bindings usable only into the in clause
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea