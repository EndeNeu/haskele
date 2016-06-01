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
