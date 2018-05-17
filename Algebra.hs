module Algebra where 



data Var        = X|Y|Z|U|V|W|R|S|T|O|P|Q|A|B|C|D|E|F|G|H|I|J|K|L|M|N deriving Show
data Mod a      = Pair a Var  
instance (Num a, Show a)=> Show (Mod a) where  
    show (Pair x v) = show x ++ show v 
data Algebra a  = Algebra [Mod a] deriving Show




data Set a = Set [a]





instance Num a => Num (Mod a) where
    (+)[(Pair n v)][(Pair m v)] = [Pair (n+m) v]
    (+)(Pair n v)(Pair m w) = Pair
