module Main where
import LmbdParser
import Exp
import System.IO
import Control.Monad
import System.Directory
import Utils
import qualified Data.Set as S

split :: String -> String -> [String]
split [] buf = if (buf == "") then [] else [buf]
split ('[':rest) buf = [buf] ++ (split rest "")
split (']':rest) buf = [buf] ++ (split rest "")
split (':':'=':rest) buf = [buf] ++ (split rest "")
split (x:rest) buf = split rest (buf++[x])

printSub :: Substitution -> String
printSub (Ok e) = show e
printSub (Error v) = "No freedom for " ++ v

main = readFile "task3.in" >>= (return . f) >>= writeFile "task3.out"
  where
    f s = 
        let (a, b, c) = (split s "" !! 0, split s "" !! 1, split s "" !! 2)       
        in printSub $ substitute (parseLambda a) (b, parseLambda c) 


