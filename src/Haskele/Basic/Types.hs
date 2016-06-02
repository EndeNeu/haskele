module Types where

-- custom data types
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- pattern match on data parameters
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- data Person = Person String String Int Float String String deriving (Show)
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show, Eq) -- eq adds equality comparison for primitive types or custom defined ones.

getFirstName :: Person -> String
getFirstName p = firstName(p)
-- *Types> let p = Person { firstName = "aaa", lastName = "123", age = 10, height = 10.0, phoneNumber = "123", flavor = "1" }
-- *Types> firstName p
-- "aaa"
-- *Types> get
-- *Types> getFirstName p
-- "aaa"

-- parametrized type
data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)


data Vector a = Vector a a a deriving (Show)

-- matching vectors on the fly.
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n


-- becuase the values take no type parameter or argument constructor we can extend Enum and get succ and pred.
-- bounded allows you to get minBound :: Day and maxBound :: Day
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- type aliases
type String = [Char]