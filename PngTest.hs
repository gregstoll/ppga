module Main where

import Png

main :: IO ()
--main = putStrLn $ png [[True, True, True], [False, True, False], [False, False, False]]
main = putStrLn $ colorPng [[(0, 0, 0), (255, 255, 255), (255, 0, 0)], [(0, 0, 255), (0, 255, 0), (255, 0, 255)]]
