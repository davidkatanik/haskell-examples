module Bludiste where

type Maze = [String]
type Point = (Int, Int)
type Path = (Point, Int)

in1 = ["##########",
       "#..#.....#",
       "#..#.....#",
       "#..#.....#",
       "#..#.....#",
       "#..#.....#",   
       "#........#",
       "##########"]
       
pp :: Maze -> IO()
pp = putStr . concat . map(++"\n")

free :: Maze -> Point -> Bool 
free x (p1, p2) = ((x !! p1) !! p2 ) /= '#'

notInSolved :: [Path] -> Point -> Bool
notInSolved x (p1, p2) = null [(s1, s2) | ((s1, s2), _) <- x, s1==p1, s2==p2]

neighbour :: Path -> [Path]
neighbour ((p1, p2), price) = let newPrice = price + 1
                              in [ ((p1+1,p2),newPrice),
                                   ((p1-1,p2),newPrice),
                                   ((p1,p2+1),newPrice),
                                   ((p1,p2-1),newPrice)]

-- path [(0,0),0] in1 []
find :: [Path] -> Maze -> [Path] -> [Path]
find [] _ x = x
find ((p , price):xs) m solved = 
  let test = (free m p) && (notInSolved solved p) 
  in if test then find (xs ++ (neighbour (p,price))) m ((p, price):solved) 
     else (find xs m solved)
     
steps :: Maze -> Point -> Point -> Int
steps m start end = let xs = find [(start, 0)] m []
                    in [ price | (p, price) <- xs, p == end] !! 0
                    
createPath :: [Path] -> Point -> Path -> [Point]
createPath solution start (p, price) 
 | start == p = [start] 
 | otherwise = (createPath solution start (getNext solution (p, price)) ) ++ [p]   
 
getNext :: [Path] -> Path -> Path
getNext solution old = [ new | new <- solution, elem old (neighbour new)] !! 0               
                    
path :: Maze -> Point -> Point -> [Point]
path m start end = let xs = find [(start, 0)] m []
                       endPath = [(p,price) | (p,price)<-xs, p == end] !! 0
                   in createPath xs start endPath
                   

putCharInMaze :: Maze -> Point -> Char -> Maze
putCharInMaze m (p1,p2) c = (take p1 m) ++ [(putCharInString (m !! p1) p2 c)] ++ (drop (p1+1) m)

putCharInString :: String -> Int -> Char -> String
putCharInString s p c = (take p s) ++ [c] ++ (drop (p+1) s)

putPath :: Maze -> [Point] -> Maze
putPath m [] = m
putPath m (x:xs) = putPath (putCharInMaze m x 'x') xs
     