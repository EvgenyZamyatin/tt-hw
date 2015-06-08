module Main where
import LmbdParser
import Exp
import System.IO
import Control.Monad
import System.Directory
import Utils
import Unifier
import qualified Data.Map as M

main = readFile "task6.in" >>= (return . f) >>= writeFile "task6.out" 
  where
    f s = 
        let l = parseLambda s in
            case typeCalc l of 
              Error e -> e
              Ok m -> showTypeExp $ extract $ M.lookup "t0" m


