{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Unifier where
import Data.Char
import qualified Data.Map as M
import Utils
import Exp
import LmbdParser
type Args = [Term]
type Name = String
data Term = TFun Name Args
          | TVar Name
         
type TypeExp = Term
instance Show Term where
    show (TFun f args) = f ++ (show args)
    show (TVar v) = v

instance Eq Term where
    TVar a == TVar b = a == b
    TFun f1 a1 == TFun f2 a2 = f1 == f2 && a1 == a2
    a == b = False

instance Substitutable Term where
    substitute (TVar v) (s, t) = if (v == s) then Ok t else Ok $ TVar v
    substitute (TFun f args) (s, t) = return $ TFun f $ map (\x->case substitute x (s,t) of Ok e -> e) args

type Equalation = (Term, Term)
type Unification = MaybeError (M.Map Name Term)

instance Show Unification where 
    show (Error e) = e
    show (Ok map) = show' $ M.toList map
      where
        show' [] = ""
        show' ((v, t):rest) = v ++ " = " ++ (show t) ++ "\n" ++ (show' rest)

isAoN c = c == '\'' || (isAlpha c) || (isNumber c)

parseName :: String -> (Name, String)
parseName = span isAoN 

parseArgs :: String -> (Args, String) 
parseArgs = parseArgs' 0
  where 
    parseArgs' x ('(':')':s) = ([], s)
    parseArgs' x ('(':s) = let (a, b) = parseTerm' s in 
                           let (x, y) = parseArgs' 1 b in
                               (a:x, y)
    parseArgs' x (',':s) = if (x == 1) then parseArgs' 1 ('(':s) else ([], ',':s)
    parseArgs' x (')':s) = if (x == 1) then ([], s) else ([], ')':s)
    parseArgs' x (c:s) = ([], c:s)

removeGaps [] = []
removeGaps (' ':s) = removeGaps s
removeGaps (x:s) = (x:(removeGaps s))

parseTerm :: String -> Term
parseTerm s = let (a,b) = (parseTerm' $ removeGaps s) in a

parseTerm' (x:s) 
  | (x >= 'i' && x <= 'z') = let (a,b) = parseName (x:s) in (TVar a, b) 
  | (x >= 'a' && x <= 'h') = let (name, rest) = parseName (x:s) in 
                             let (args, rest') = parseArgs rest in
                                 (TFun name args, rest')


findVars :: Term -> [Name]
findVars = uniquify . findVars'
  where      
    findVars' (TVar v) = [v]
    findVars' (TFun name a) = foldr (\e lst -> (findVars' e) ++ lst) [] a


brush :: Unification -> Unification
brush (Error x) = Error x
brush (Ok m) = 
    let extr (Ok s) = s in
    let frs (a,b) = a in
    let scn (a,b) = b in
    let lst = M.toList m in
    let nlst = foldr (\w z -> map (\pr -> ((frs pr), extr $ substitute (scn pr) w)) z) lst lst in
        if (nlst == lst) 
           then Ok $ M.fromList lst
           else brush $ Ok $ M.fromList nlst


robOccursCheck :: Name -> Term -> M.Map Name Term -> Bool
robOccursCheck x t m = foldr f True (findVars t)
  where
    f var b = if (var == x)
                 then False
                 else case M.lookup x m of
                        Just e -> b && robOccursCheck x e m
                        Nothing -> b

unifyTerms :: [Equalation] -> Unification
unifyTerms = brush . (unifyTerms' M.empty)
unifyTerms' :: M.Map Name Term -> [Equalation] -> Unification
unifyTerms' map = foldr (\eq map -> map >>= f eq) (return map)
  where 
    get :: Term -> M.Map Name Term -> Term
    get (TVar v) map = let res = M.lookup v map 
                        in case res of 
                             Nothing -> TVar v
                             Just e -> get e map
    get a map = a
    f :: Equalation -> M.Map Name Term -> Unification
    f (a, b) map =
        let x = get a map in
        let y = get b map in
            if (x == y) 
               then return map
               else case (x,y) of
                      (TVar u, TVar w) -> return $ M.insert u (TVar w) map
                      (TVar u, w) -> if (robOccursCheck u w map) 
                                        then return $ M.insert u w map
                                        else Error "Can't unify"
                      (w, TVar u) -> if (robOccursCheck u w map) 
                                        then return $ M.insert u w map
                                        else Error "Can't unify"
                      (TFun f1 ars1, TFun f2 ars2) -> 
                        if (f1 /= f2 || (length ars1 /= length ars2))
                           then Error "Can't unify"
                           else unifyTerms' map (zip ars1 ars2)
    


genMap :: Int -> [Name] -> (Int, M.Map Name TypeExp)
genMap x (s:r) = let (a,b) = genMap (x+1) r in
                     (a+1, M.insert s (TVar $ "t"++(show a)) b)
genMap x [] = (x, M.empty)
 
showTypeExp (TFun f [TFun x y, b]) = "(" ++ (showTypeExp $ TFun x y) ++ ")" ++ "->" ++ (showTypeExp b)
showTypeExp (TFun f [TVar v, b]) = v ++ "->" ++ (showTypeExp b)
showTypeExp (TVar v) = v

typeCalc :: Exp -> Unification
typeCalc e = 
    let (c, map) = (genMap 1 $ findFreeVars e) in
    let (a, b) = eqsCalc c map e (TVar "t0") in
        unifyTerms b

extract (Just x) = x

eqsCalc :: Int -> M.Map Name TypeExp -> Exp -> TypeExp -> (Int, [Equalation])
eqsCalc tpCnt map (Var x) tp = (tpCnt+1, [(tp, extract $ M.lookup x map)])  
eqsCalc tpCnt map (App a b) tp = 
    let ntp = TVar $ "t" ++ (show tpCnt) in
--    let (tpCnt', ma) = genMap (tpCnt+1) (findFreeVars a) in
--    let (tpCnt'', mb) = genMap tpCnt' (findFreeVars b) in
    let (tpCnt', ansa) = eqsCalc (tpCnt+1) map a $ TFun "f" [ntp, tp] in
    let (tpCnt'', ansb) = eqsCalc tpCnt' map b ntp in
        (tpCnt'', ansa++ansb)
eqsCalc tpCnt map (Lmbd x e) tp = 
    let tp1 = TVar $ "t" ++ (show tpCnt) in
    let tp2 = TVar $ "t" ++ (show $ tpCnt+1) in
    let nmap = M.insert x tp1 map in
    let (tpCnt', ans) = eqsCalc (tpCnt+2) nmap e tp2 in
        (tpCnt', ans ++ [(tp, TFun "f" [tp1, tp2])])











