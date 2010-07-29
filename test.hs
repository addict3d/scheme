module Main where

import System.Environment
import Monad


main :: IO ()
main = getArgs >>= (\args ->  putStrLn ((args !! 0) ))
