module Main where
import LmbdParser
import Exp
import System.IO
import Control.Monad
import System.Directory
import Utils
import Unifier
import qualified Data.Set as S

splitByEq :: String -> String -> [String]
splitByEq buf [] = [buf]
splitByEq buf ('=':s) = [buf]++(splitByEq "" s)
splitByEq buf (x:s) = splitByEq (buf++[x]) s

main = readFile "task5.in" >>= (return . show . f) >>= writeFile "task5.out" 
  where
    g str = (parseTerm . head $ splitByEq "" str, parseTerm . last $ splitByEq "" str)
    f s = let lst = map g (lines s) in unifyTerms lst
