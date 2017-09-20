module Main where

import Lib
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "Filename is needed"
    else withFile (head args) ReadMode $ \fp -> do
    prog <- hGetContents fp
    exec prog
