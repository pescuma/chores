module NextGen where

infixl 1 &
(&) :: a -> (a -> b) -> b
a & f = f a


