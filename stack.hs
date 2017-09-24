module Stack 
    ( Stack,
      push,
      pop,
      top,
      isEmpty
    ) where

push :: a -> Stack a -> Stack a
pop :: Stack a -> Stack a
top :: Stack a -> a
isEmpty :: Stack a -> Bool
    
newtype Stack a = St [a] 

push x (St y) = St (x:y)
pop (St (x)) = St (x)
top (St x) = head x
isEmpty (St x) = null x