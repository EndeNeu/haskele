module Let where

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

