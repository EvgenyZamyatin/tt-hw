module Main where
import LmbdParser
import Exp
import System.IO
import Control.Monad
import System.Directory
import Utils
import qualified Data.Set as S

main = readFile "task4.in" >>= (return . show . reduceMeAll . parseLambda) >>= writeFile "task4.out" 
