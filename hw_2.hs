module Domaci_ukol_2 where

import Prelude
import Data.Char
import Data.List

----         Zadani         ----  
-- du: pekne zarovnana uctenka, povoleno rozmer je konstantni... 
-- muzeme funkce jake chceme.. a kolik chceme
-- |chleba .... 15|
-- |rohlik ....  6|
-- |--------------|
-- |celkem .... 21|
-- chleba 15 kc
-- rohlik 5kc
----------------------------------
billLength :: Int
billLength = 50
billContentLength :: Int
billContentLength = 40

bcs :: TillType 
bcs = [
        1000,
        1004,
        1002,
        1001
      ]

db :: Database
db = [
      (1000,"champagne",1999),
      (1001,"Red vine",199),
      (1002,"White vine",99),
      (1003,"Whiskey",5200),
      (1004,"Bourbon",2000),
      (1005,"Vodka",500),
      (1006,"Pilsner Urquell",40)
    ]
----------------------------------


type Name = String
type Price = Int
type BarCode = Int
type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [(Name,Price)]

----------------------------------

-- Makes bill for till
makeBill :: TillType -> BillType
makeBill [barcode] = findSource barcode db
makeBill (barcode:barcodes) = (findSource barcode db) ++ (makeBill barcodes)

-- Formats bill
formatBill :: BillType -> String
formatBill bt = concat
  ( intersperse "\n" (  
    [
      generateRow "-" billLength, 
      generateRowSentence "Hospudka U Supermana", 
      generateRowSentence "", 
      generateRowSentence "       **************************       ",
      generateRowSentence "    .*##*:*####***:::**###*:######*.    ",
      generateRowSentence "   *##: .###*            *######:,##*   ",
      generateRowSentence " *##:  :####:             *####*.  :##: ",
      generateRowSentence "  *##,:########**********:,       :##:  ",
      generateRowSentence "   .#########################*,  *#*    ",
      generateRowSentence "     *#########################*##:     ",
      generateRowSentence "       *##,        ..,,::**#####:       ",
      generateRowSentence "        ,##*,*****,        *##*         ",
      generateRowSentence "          *#########*########:          ",
      generateRowSentence "            *##*:*******###*            ",
      generateRowSentence "             .##*.    ,##*              ",
      generateRowSentence "               :##*  *##,               ",
      generateRowSentence "                 *####:                 ",
      generateRowSentence "                   :,                   ",
      generateRowSentence "",   
      generateRowSentence "",        
      generateRowSentence "Bylo nam poteseni Vas obslouzit",
      generateRowSentence "Obsluhoval Vas David Katanik",
      generateRowSentence "",   
      generateRowSentence "",      
      generateRowSentence "Vase celkova utrata",
      generateRowSentence (generateRow "-" billContentLength),
      generateRowSentence ""
    ] 
    ++
    [ (generateRowSentence(generateSentenceWithFillString name (show price) "." billContentLength)) | (name, price) <- bt]
    ++
    [
      generateRowSentence "",    
      generateRowSentence "----------------------------------------",
      generateRowSentence (generateSentenceWithFillString "Cena celkem s DPH" (show (sumOfBill bt)) "." billContentLength), 
      generateRowSentence "",      
      generateRow "-" billLength,
      "\n"
    ]
  )) 

-- Produces bill
produceBill :: TillType -> String
produceBill = formatBill. makeBill

----------------------------------

-- Finds source in database
findSource :: BarCode -> Database -> BillType
findSource barcode database = [ (name,price) | (bc, name, price) <- database, barcode == bc] 

-- Generates row of input string
generateRow :: String -> Int -> String
generateRow str len = concat [ str | x <- [1..len]]

-- Generate string of spaces with given Int parameter
generateFreeSpaces :: Int -> String
generateFreeSpaces num = concat [ " " | x <- [1..num]]

-- Generates sentece |  sentence  |
generateRowSentence :: String -> String
generateRowSentence sentence = concat 
  [
    "|",
    generateFreeSpaces (if (((length sentence) `mod` 2) == 0) then (getNumberOfFreeCharacters sentence) else ((getNumberOfFreeCharacters sentence)+1)),
    sentence,
    generateFreeSpaces (getNumberOfFreeCharacters sentence),
    "|"
  ]
  
-- Generates sentence with left side, right side, fill string and size
generateSentenceWithFillString :: String -> String -> String -> Int -> String
generateSentenceWithFillString left right fill size =  left ++ (concat ([ fill | x <- [1..(size-(length left)-(length right))] ])) ++ right

-- Gets or calculates number of character which program has to fill on each side of sentence
getNumberOfFreeCharacters :: String -> Int
getNumberOfFreeCharacters str = (((billLength-2) - (length str)) `div` 2)  


-- Counts sum of a BillType
sumOfBill :: BillType -> Int
sumOfBill [] = 0
sumOfBill [(name, price)] = price
sumOfBill ((name, price):xs) = price + sumOfBill xs


-- Prints array of String
pp :: [String] -> IO ()
pp = putStr . concat . map (++"\n")