module Main where

import MakePng
import qualified Data.Map as M
import System

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

appendToMap :: M.Map String String -> String -> M.Map String String
appendToMap map str = M.insert (valSplit !! 0) (valSplit !! 1) map
                      where valSplit = split str '='

getDataFromQueryString :: String -> M.Map String String
getDataFromQueryString s = foldl (\m pair -> appendToMap m pair) M.empty (split s '&')

main :: IO ()
main = do args <- getArgs
          let dat = getDataFromQueryString (head args)
              w = read $ M.findWithDefault "0" "w" dat
              h = read $ M.findWithDefault "0" "h" dat
              f = M.findWithDefault "" "f" dat
          putStrLn $ getFunctionPPMString w h (parseIncomingJSON f)
