module Exp where

data Exp = App Exp Exp
         | Lmbd String Exp    
         | Var String

instance Show Exp where
    show e = show' e
        where
        show' (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
        show' (Lmbd x e) = "(" ++ "\\" ++ x ++ "." ++ (show e) ++ ")"
        show' (Var x) =  x       

instance Eq Exp where
    (App a1 b1) == (App a2 b2) = (a1 == a2) && (b1 == b2)
    (Var x) == (Var y) = x == y
    (Lmbd v1 e1) == (Lmbd v2 e2) = (v1 == v2) && (e1 == e2)
    x == y = False 
