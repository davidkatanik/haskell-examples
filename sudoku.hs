-- https://www.algoritmy.net/article/1351/Sudoku
-- http://en.wikipedia.org/wiki/Sudoku
import Data.Array
import Debug.Trace

-------------------------------------------------------
-- Vstupni data
-------------------------------------------------------
pr1 :: [[HodnotPole]]
pr1 =           [
                    [5, 3, 0,   0, 7, 0,   0, 0, 0],
                    [6, 0, 0,   1, 9, 5,   0, 0, 0],
                    [0, 9, 8,   0, 0, 0,   0, 6, 0],

                    [8, 0, 0,   0, 6, 0,   0, 0, 3],
                    [4, 0, 0,   8, 0, 3,   0, 0, 1],
                    [7, 0, 0,   0, 2, 0,   0, 0, 6],

                    [0, 6, 0,   0, 0, 0,   2, 8, 0],
                    [0, 0, 0,   4, 1, 9,   0, 0, 5],
                    [0, 0, 0,   0, 8, 0,   0, 7, 0]
                ]

pr2 :: [[HodnotPole]]
pr2 =           [
                    [0, 0, 9,   0, 0, 0,   7, 6, 0],
                    [4, 0, 0,   0, 0, 6,   0, 0, 0],
                    [7, 0, 3,   0, 0, 5,   0, 0, 9],

                    [2, 0, 0,   4, 0, 8,   0, 5, 6],
                    [0, 0, 0,   0, 0, 0,   0, 0, 0],
                    [6, 1, 0,   3, 0, 7,   0, 0, 4],

                    [1, 0, 0,   2, 0, 0,   4, 0, 3],
                    [0, 0, 0,   7, 0, 0,   0, 0, 1],
                    [0, 9, 8,   0, 0, 0,   2, 0, 0]
                ]
-------------------------------------------------------
-- Datove typy
-------------------------------------------------------
-- Hodnota pole.. 0 znamena nevyplneno
type HodnotPole = Int
-- Souradnice pole.. Radek x Sloupec
type SouradnicePole = (Int, Int)
-- Sudoku cele hraci pole 9x9. Skupina je 3x3
type Hra = Array SouradnicePole HodnotPole
-------------------------------------------------------
-- Spousteci funkce main
-------------------------------------------------------
main = do
    let reseni = najdiReseni vytvorHru
    vypisHru reseni
-------------------------------------------------------
-- Funkcionalita
-------------------------------------------------------
-- Vytvori hru
vytvorHru :: Hra
vytvorHru = array ((0, 0), (8, 8)) (premenRadky pr2)

-- Transformuje radky na pole dvojic (soradnice,hodnota). 
premenRadky :: [[HodnotPole]] -> [(SouradnicePole, HodnotPole)]
-- Pro kazdy radek 
premenRadky = concatMap premenRadek . zip [0..8]
  where
    premenRadek :: (Int, [HodnotPole]) -> [((Int, Int), HodnotPole)]
    -- Vstup :0 [0,0,9,0,0,0,7,6,0])
    -- zip vsech hodnot do radku [(0, h1),(0, h2),]
    premenRadek (radek, hodnoty) = prirazeniSloupce radek (zip [0..8] hodnoty)

    prirazeniSloupce :: Int -> [(Int, HodnotPole)] -> [((Int, Int), HodnotPole)]
    -- Vstup: 0 [(0,0),(1,0),(2,9),(3,0),(4,0),(5,0),(6,7),(7,6),(8,0)]
    -- Sloupces namapuje na dvojici ((radek, sloupec), hodnota)
    prirazeniSloupce radek sloupecs = map (\(sloupec, h) -> ((radek, sloupec), h)) sloupecs


-- Vypise hru
-- Vypise zakladni hru --> vypisHru (Just vytvorHru)
vypisHru :: Maybe Hra -> IO ()
vypisHru Nothing  = putStrLn "Zadne reseni"
-- mapM_ nezajimame se o vystup reseni z http://stackoverflow.com/questions/30060399/couldnt-match-type-char-with-char
vypisHru (Just hra) = mapM_ putStrLn [show (hra `hodnotyVRadku` radek) | radek <- [0..8]]


-- Vraci prvni reseni nebo zadne kdyz ho nenajde. Maybe jsem pouzil abych zajistil reseni pro "zadne reseni"
najdiReseni :: Hra -> Maybe Hra
najdiReseni = prvniNeboNic . moznaReseni

-- Vrati prvni hodnotu nebo nic.. resi situaci zadne reseni
prvniNeboNic :: [a] -> Maybe a
prvniNeboNic []     = Nothing
prvniNeboNic (x:xs) = Just x

-- Vraci vsechna reseni
moznaReseni :: Hra -> [Hra]
-- zavola reseni pro vsechna prazdna pole a hru
moznaReseni hra = reseni (souradnicePrazdnychPoli hra) hra
  where
    -- Pro souradnice prazdnych poli a hru vrati mozne reseni hry.
    -- Urci ktere hodnoty muzou byt na souradnici a
    -- rekurzivne najde vsechna mozna reseni pro zadane hodnoty
    reseni :: [SouradnicePole] -> Hra -> [Hra]
    reseni []     hra = [hra]
    -- namapuje mozneHry k reseni
    reseni (s:souradnice) hra = concatMap (reseni souradnice) mozneHry
      where
        -- vrati vsechny mozne hodnoty pole
        mozneHodnotyPole  = [hodnota | hodnota <- [1..9], moznostZarazeniPoleNaSouradnice hodnota s hra]
        -- vrati vsechny hry se zarazenyma hodnotama
        mozneHry = let z = mozneHodnotyPole
                       x = (map (\hodnota -> zaradHodnotuNaSouradnici hodnota s hra) z)
                   in {-traceShow (z,s,x) $-} x
    

-- Vraci list souradnic prazdnych poli (tedy pole = 0)
souradnicePrazdnychPoli :: Hra -> [SouradnicePole]
-- generace pole dvojic (radek, sloupec), ktere nemaji hodnotu
souradnicePrazdnychPoli hra = [(radek, sloupec) | radek <- [0..8], sloupec <- [0..8], hra ! (radek, sloupec) == 0]

-- Zaradi hodnoty postupne na sve misto(souradnici), vyuziva inkrementacni update pro pole https://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array-IArray.html#v:-47--47-
zaradHodnotuNaSouradnici :: HodnotPole -> SouradnicePole -> Hra -> Hra
-- // je funkce z Data.array ktera vymeni v poli dvojic hodnoty za zadane: array ((0,0), (0,1)) [((0,0),1),((0,1),2)] // [((0,0),99),((0,1),-99)] array ((0,0),(0,1)) [((0,0),99),((0,1),-99)]
zaradHodnotuNaSouradnici hodnota (radek, sloupec) hra = hra // [((radek, sloupec), hodnota)]

-- Zjisti, zda je mozne zaradit pole na zadane souradnice ve hre
moznostZarazeniPoleNaSouradnice :: HodnotPole -> SouradnicePole -> Hra -> Bool
moznostZarazeniPoleNaSouradnice m (radek, sloupec) hra = neniVRadku && neniVeSloupci && neniVeSkupine
  where
    neniVRadku    = notElem m (hra `hodnotyVRadku` radek)
    neniVeSloupci = notElem m (hra `hodnotyVeSloupci` sloupec)
    neniVeSkupine = notElem m (hra `hodnotyVeSkupine` (radek, sloupec))

-- Vraci hodnoty v zadanem radku
hodnotyVRadku :: Hra -> Int -> [HodnotPole]
hra `hodnotyVRadku` radek = [hra ! dvojice | dvojice <- range((radek, 0), (radek, 8))]

-- Vraci hodnoty v zadanem sloupci
hodnotyVeSloupci ::  Hra -> Int -> [HodnotPole]
hra `hodnotyVeSloupci` sloupec = [hra ! dvojice | dvojice <- range((0, sloupec), (8, sloupec))]

-- Vraci skupinu hodnot v matici 3x3 pro zadane souradnice ve hre
hodnotyVeSkupine :: Hra -> SouradnicePole -> [HodnotPole]
hra `hodnotyVeSkupine` (radek, sloupec) = [hra ! dvojice | dvojice <- souradnice]
  where
    radekR = (radek `div` 3) * 3
    sloupecS = (sloupec `div` 3) * 3
    souradnice = range((radekR, sloupecS), (radekR + 2, sloupecS + 2))



