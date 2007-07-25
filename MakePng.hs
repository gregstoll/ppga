module MakePng where

import System.IO
import JSON

parseIncomingJSON :: String -> Maybe Value
parseIncomingJSON s = parse $ map singleQuoteToDouble s

singleQuoteToDouble :: Char -> Char
singleQuoteToDouble '\'' = '"'
singleQuoteToDouble c = c
