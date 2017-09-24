-- domaci ukol 3
-- words oseka string po slovech
-- pokud chceme co nejmene radku.. 
-- formatovani do bloku.. vyvazeny pocet mezer..

module Domaci_ukol_3 where

import Prelude
import Data.Char
import Data.List


lineLength = 30


textData :: String
textData = "In computer science, functional programming is a programming \
\paradigm that treats computation as the evaluation of mathematical functions \
\and avoids state and mutable data. It emphasizes the application of functions, in contrast to \
\the imperative \
\programming style, which emphasizes changes in state."


justifyText :: String -> String
justifyText [] = ""
justifyText str = justifyLines (processString (words str) 0)

justifyLines :: String -> String
justifyLines [] = ""
justifyLines str = fillLines (lines str)

fillLines :: [String] -> String
fillLines [] = []
fillLines (x:xs) = (processLine x) ++ (fillLines xs)


processString :: [String] -> Int -> String
processString [] _ = "\n"
processString (x:xs) len | ((length x) + len) < lineLength = x ++ " " ++ (processString xs ((length x) + len + 1))
                                  | otherwise = "\n" ++ x ++ " " ++ (processString xs (length x))

processLine :: String -> String
processLine str = concat (mergeLists (words str) (createSpacesList (length str) (length (words str)))) ++ "\n"

createSpacesList :: Int -> Int -> [String]
createSpacesList len count = mergeLists [concat [" " | x <- [1..(calculateSpaces (len) (count) `div` (count-1))]] | tmp <- [1..(count-1)]] [" " | x <- [1..(calculateSpaces (len) (count) `mod` (count-1))]] 



-- lineWidth - lineLength + countOfWords + 1
calculateSpaces :: Int -> Int -> Int
calculateSpaces len count = lineLength - len + count - 1


-- Help functions
mergeLists :: [String] -> [String] -> [String]
mergeLists [] _ = []
mergeLists list [] = list
mergeLists (x:xs) (y:ys) = [x++y] ++ (mergeLists xs ys) 