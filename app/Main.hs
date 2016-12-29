module Main where

import           Lib

main :: IO ()
main = timeit (remoteSquareSumAsync4 [3, 4]) >>= print
