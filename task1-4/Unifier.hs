{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Unifier where
import Data.Char
import qualified Data.Map as M
import Utils
type Args = [Term]
type Name = String

data Term = TFun Name Args
          | TVar Name
         
instance Show Term where
    show (TFun f args) = f ++ (show args)
    show (TVar v) = v


instance Eq Term where
    TVar a == TVar b = a == b
    TFun f1 a1 == TFun f2 a2 = f1 == f2 && a1 == a2
    a == b = False
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


robOccursCheck :: Name -> Term -> M.Map Name Term -> Bool
robOccursCheck x t m = foldr f True (findVars t)
  where
    f var b = if (var == x)
                 then False
                 else case M.lookup x m of
                        Just e -> b && robOccursCheck x e m
                        Nothing -> b

unifyTerms :: [Equalation] -> Unification
unifyTerms = unifyTerms' M.empty
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
    









