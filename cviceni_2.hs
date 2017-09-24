module Cviceni2 where

import Prelude
import Data.Char

----------------------------------
doubleAll xs = [x*x | x <- xs]

toUpperCase :: String -> String
toUpperCase xs = [ toUpper x  | x <- xs]

getDivisors :: Int -> [Int]
getDivisors n = [ x | x <- [1..n], mod n x == 0 ]

isPrime n = length [ x | x <- [1..n], mod n x == 0 ] <= 2
----------------------------------
type Person = String
type Book = String
type Database = [(Person,Book)]

d1 = [("Marek","Babicka"),("Marek","Haskell"),("Jana","Haskell")]

books :: Database -> Person -> [Book]
books db person = [ b | (p,b) <- db, person == p]

borrowers :: Database -> Book -> [Person]
borrowers db book = [p | (p,b) <- db, book == b]

borroved :: Database -> Book -> Bool
borroved db book = length [p | (p,b) <- db, book == b] > 1 

numBorroved :: Database -> Book -> Int
numBorroved db book = length [p | (p,b) <- db, book == b]

makeLoan :: Database -> Person -> Book -> Database
makeLoan db person book = db ++ [(person,book)]

returnLoan :: Database-> Person -> Book -> Database
returnLoan db person book = [(p,b) | (p,b) <- db, if book==b then if person==p then False else True else True]
----------------------------------                                
type Pic = [String]
obr :: Pic
obr = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####",
        "....#####"
      ]
-- Printing of nice picture      
pp :: Pic -> IO ()
pp = putStr . concat . map (++"\n")

--flips picture horizontaly;
flipH :: Pic -> Pic
flipH pic = [reverse x | x <- pic]
-- nebo s map(reverse)

--flips picture veriticaly;
flipV :: Pic -> Pic
flipV pic = reverse [x |x <- pic]
-- nebo jenom reverseeee

--place one picture above another (considering they have the same width)
above :: Pic -> Pic -> Pic
above obrA obrB = obrA ++ obrB

--place two pictures side by side (considering they have the same height);
sideBySide :: Pic -> Pic -> Pic
sideBySide obrA obrB = zipWith (++) obrA obrB
-- bez parametru zipWith (++) pouze

--rotate picture to the left and to the right.
-- get row and make from it column

swapRowToColumn [] = []
swapRowToColumn (x:xs) = [x] : swapRowToColumn xs

rotateL :: Pic -> Pic
rotateL [x] = reverse (swapRowToColumn x)
rotateL (x:xs) = reverse (swapRowToColumn x ) `sideBySide` (rotateL xs)

rotateR :: Pic -> Pic
rotateR [x] = swapRowToColumn x
rotateR (x:xs) = (rotateR xs) `sideBySide`  (swapRowToColumn x )


