module Main where

import System.Environment
import Compiler
import Interpreter

--TODO Task 3.4
main :: IO ()
main = do
    a <- getArgs
    let c = read (head a) :: Com
    print (ccomp c)