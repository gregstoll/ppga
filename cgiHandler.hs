module Main where

import Network.CGI
import Network.CGI.Protocol (formDecode, cgiRequestBody)
import MakePng

inputToInt :: Maybe String -> Int
inputToInt t = maybe 0 read t

cgiMain :: CGI CGIResult
cgiMain = do width <- getInput "w"
             height <- getInput "h"
             fn <- getInput "f"
             setHeader "Content-Type" "image/x-portable-pixmap"
             output (getFunctionPPMString (inputToInt width) (inputToInt height) (parseIncomingJSON (maybe "" id fn)))

main :: IO ()
main = runCGI (handleErrors cgiMain)
