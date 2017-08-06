module Complex where


-- Define Complex Number
data C = C Double Double 
instance Show C where show x = showC x
showC (C x 0) = show x
showC (C 0 y) = show y ++ "i"
showC (C x y) = if y < 0 
        then show x ++ show y ++ "i"
        else show x ++ "+" ++ show y ++ "i"
instance Num C where 
    (C x y)*(C a b) = C (x*a-y*b) (x*b+y*a)
    (C x y)+(C a b) = C (x+a)     (y+b)
    (C x y)-(C a b) = C (x-a)     (y-b)
    negate (C x y)  = C (-x)      (-y)
    abs    (C x y)  = C (x^2+y^2) 0
    signum (C x y)  = C (signum x)(signum y)
    fromInteger x   = C (fromInteger x) 0
