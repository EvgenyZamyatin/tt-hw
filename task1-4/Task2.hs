module Main where
import LmbdParser
import Exp
import System.IO
import Control.Monad
import System.Directory
import Utils
import qualified Data.Set as S

main = readFile "task1.in" >>= (return . f) >>= writeFile "task2.out"
  where
      f = print . findFreeVars . parseLambda 
        where
            print [] = ""
            print (s:x) = s ++ "\n" ++ print x


