module Cviceni_4 where

data Color = Black | White

class Visible a where
  toString :: a -> String
  size :: a -> Int
  size = length . toString
  
instance Visible Char where
  toString ch = [ch]
  size _ = 1
  
instance Visible a => Visible [a] where
  toString = concat . map toString          
  size = foldr (+) 0 . map size             

instance Visible Bool where
  toString True = "True"
  toString False = "False"
  
instance Visible Color where
  toString Black = "Black"
  toString White = "White"

instance (Visible a, Visible b) => Visible (a, b) where
  toString (x,y) = (toString x)++","++(toString y)
  
instance (Visible a) => Show a where
  showPrec x = show x
  show x = toString x

showText :: (Visible a) => a -> String
showText a = toString a