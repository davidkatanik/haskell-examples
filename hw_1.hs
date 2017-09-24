module Domaci_ukol_1 where

-- Domaci ukol: 1,2,3(zip),4,5(zip with),6(suma soucinu jednotlivych slozek),7(kazdy s kazdym),8(nejmensi element)
-- zip spojuje seznamy
-- zip with aplikuje operaci
-- map aplikuje funkci na kazdy prvek v seznamu
-- import Prelude hiding(sum)      

-- 1. Create a function that computes length of a list.
listLength :: [t] -> Int
listLength [] = 0
listLength (_:list) = 1 + listLength list

-- 2. Create a function that merge two lists into one list.
mergeLists :: [t] -> [t] -> [t]
mergeLists list [] = list
mergeLists list (x:xs) = mergeLists (list ++ [x]) xs

-- 3. Create a function that merge two lists into one list of tuples.
mergeListsToTuples :: [t1] -> [t2] -> [(t1,t2)]
mergeListsToTuples _ [] = []
mergeListsToTuples [] _ = []
mergeListsToTuples (x:xs) (y:ys) = (x,y):mergeListsToTuples xs ys

-- 4. Create a function that reverse a list.
reverseList :: [t] -> [t]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- 5. Create a function that merges two lists into one using given function to merge specific elements together. 
--    Given lists: [1,2,3] [1,2,3] and operation *, the result should be [1,4,9].
mergeListsWith :: [t1] -> [t2] -> (t1 -> t2 -> t3) -> [t3]
mergeListsWith _ [] _ = []
mergeListsWith [] _ _ = []
mergeListsWith (x:xs) (y:ys) op = op x y: mergeListsWith xs ys op
 
 -- 6. Create a function that product scalar multiplication if two vectors.
scalarMultiple :: (Num t) => [t] -> [t] -> t
scalarMultiple _ [] = 0
scalarMultiple [] _ = 0
scalarMultiple (x:xs) (y:ys) = x*y + (scalarMultiple xs ys) 



-- 7. Create a function that compute Cartesian product of two vectors.

cartesianProduct :: [t1] -> [t2] -> [(t1, t2)]
cartesianProduct _ [] = []
cartesianProduct [] _ = []
cartesianProduct (x:xs) (ys) = (oneCartesianProduct x ys) ++ (cartesianProduct xs ys)        

oneCartesianProduct :: t1 -> [t2] -> [(t1,t2)]
oneCartesianProduct _ [] = []
oneCartesianProduct x (y:ys) = (x,y): oneCartesianProduct x ys

cartesianProduct2 :: [t1] -> [t2] -> [(t1, t2)]
cartesianProduct2 _ [] = []
cartesianProduct2 [] _ = []
cartesianProduct2 xs ys = [(x,y) | x <- xs, y <- ys]

-- 8. Create a function that find the smallest element in the list. Consider input restrictions.
smallestInList :: (Ord t) => [t] -> t
smallestInList (x:xs) = smallestItemInList x xs

smallestItemInList :: (Ord t) => t -> [t] -> t
smallestItemInList x [] = x
smallestItemInList x (y:ys) 
  | x <= y = smallestItemInList x ys
  | otherwise = smallestItemInList y ys

smallestInList2 :: (Ord t) => [t] -> t
smallestInList2 [] = error "Empty list"
smallestInList2 [x] = x
smallestInList2 (x:y:xs) = smallestInList2 ((if x < y then x else y):xs)




