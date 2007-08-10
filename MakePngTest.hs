module Main where

import MakePng
import JSON

main :: IO ()
--main = putStrLn $ show $ parseIncomingJSON "{'t':'sin','ch':[{'t':'sub','m':'c','ch':[{'t':'sin','ch':[{'t':'neg','ch':[{'t':'atan','m':'c','ch':[{'t':'add','m':'w','ch':[{'t':'cchsl','ch':[{'t':'x'},{'t':'y'},{'t':'x'}]},{'t':'log','m':'w','ch':[{'t':'y'}]}]}]}]}]},{'t':'cos','ch':[{'t':'abs','ch':[{'t':'rd','ch':[{'t':'y'}]}]}]}]}]}"
main = putStrLn $ show $ evalFunction 5 5 (parseIncomingJSON "{'t':'x'}")
