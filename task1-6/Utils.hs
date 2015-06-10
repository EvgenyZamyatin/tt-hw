module Utils where
import LmbdParser
import Exp
import System.IO
import Control.Monad
import System.Directory
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L


data MaybeError a = Ok a
                  | Error String

type Substitution = MaybeError Exp


instance Monad MaybeError where
    return = Ok 
    s >>= g = case s of 
                Error v -> Error v
                Ok e -> g e

class Substitutable a where 
    substitute :: a -> (String, a) -> MaybeError a

instance Substitutable Exp where
    substitute = substitute' S.empty
      where
        substitute' :: S.Set String -> Exp -> (String, Exp) -> Substitution
        substitute' set (App a b) p = 
            (substitute' set a p) >>= 
                \x -> (substitute' set b p >>= \y -> return (App x y))
        substitute' set (Lmbd x e) p = 
            (substitute' (S.insert x set) e p) >>= return . (\res->Lmbd x res)
        substitute' set (Var x) (t, e) = 
            if (x /= t || S.member x set) 
                then return (Var x) 
                else if (((S.toList set) `L.intersect` (findFreeVars e)) == [])
                then return e
                else Error $ head ((S.toList set) `L.intersect` (findFreeVars e))



findFreeVars :: Exp -> [String]
findFreeVars e = uniquify $ findFreeVars' S.empty e
  where  
    findFreeVars' :: S.Set String -> Exp -> [String]
    findFreeVars' set (App a b) = (findFreeVars' set a) ++ (findFreeVars' set b)
    findFreeVars' set (Lmbd v e) = findFreeVars' (S.insert v set) e
    findFreeVars' set (Var x) = if (S.member x set) then [] else [x]

uniquify :: Ord a => [a] -> [a]
uniquify = S.toList . S.fromList



patch :: Exp -> Exp -> Exp
patch (App a b) p = App (patch a p) (patch b p)
patch (Var x) p = Var x
patch (Lmbd x e) p = cont (foldr f (Lmbd x e) (findFreeVars p)) p
  where  
    cont (Lmbd x e) p = Lmbd x $ patch e p
    f var (Lmbd x e) = if (x /= var) 
                        then (Lmbd x e)
                        else hang' (Lmbd x e)

hang' (Lmbd x e) = tmp (x++"'") (Lmbd x e)
  where 
    tmp nw (Lmbd x e) = case substitute e (x, Var nw) of
                         Error s -> tmp (nw++"'") (Lmbd x e)
                         Ok ans  -> (Lmbd nw ans)

reduceMeAll :: Exp -> Exp
reduceMeAll e = if (e == (reduce e)) then e else reduceMeAll $ reduce e

reduce :: Exp -> Exp
reduce (Var v) = Var v
reduce (Lmbd v e) = Lmbd v $ reduce e
reduce (App (Lmbd v e) a) = let patched = patch e a in 
                                extract $ substitute patched (v, reduce patched)
  where
    extract (Ok e) = e
reduce (App x y) = App (reduce x) (reduce y)



