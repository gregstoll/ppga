module Main where

import Png
import System.IO
import qualified Data.ByteString as BS

main :: IO ()
--main = putStrLn $ png [[True, True, True], [False, True, False], [False, False, False]]
--main = putStrLn $ colorPng [[(0, 0, 0), (255, 255, 255), (255, 0, 0)], [(0, 0, 255), (0, 255, 0), (255, 0, 255)]]
main = do handle <- openFile "readtest.png" ReadMode
          bstring <- BS.hGetContents handle
          hClose handle
          putStrLn $ show bstring
          sequence_ $ map (putStrLn . show) (readPNGChunks (BS.drop 8 bstring))
