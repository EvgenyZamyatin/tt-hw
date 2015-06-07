module Main where
import LmbdParser
import Exp
import System.IO
import Control.Monad
import System.Directory


main = readFile "task1.in" >>= (return . show . parseLambda) >>= writeFile "task1.out"

