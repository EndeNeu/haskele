module RecursiveDataType where

-- cons recourses the List data type.
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

