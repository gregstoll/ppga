module MakePng where

import System.IO
import JSON
import qualified Data.Map as M
import qualified Codec.Compression.Zlib as Z

data Fn = Num | X | Y | Atan | Abs | Cos | Exp | Log | Neg | Rd | Ru | Sin | Add | Div | Mul | Sub | Ccrgb | Cchsl | Unknown deriving (Enum, Eq)
instance Read Fn where
    readsPrec d s = [(strToFn s, "")]
instance Show Fn where
    show = fnToStr
data Mapping = Clip | Wrap | UnknownMapping
instance Read Mapping where
    readsPrec d s = [(strToMapping s, "")]
instance Show Mapping where
    show Clip = "clip"
    show Wrap = "wrap"
    show UnknownMapping = "???"
type FullFn = M.Map String Value

strToMapping :: String -> Mapping
strToMapping "w" = Wrap
strToMapping "c" = Clip
strToMapping _ = UnknownMapping

strToFn :: String -> Fn
strToFn "num" = Num
strToFn "x" = X
strToFn "y" = Y
strToFn "atan" = Atan
strToFn "abs" = Abs
strToFn "cos" = Cos
strToFn "exp" = Exp
strToFn "log" = Log
strToFn "neg" = Neg
strToFn "rd" = Rd
strToFn "ru" = Ru
strToFn "sin" = Sin
strToFn "add" = Add
strToFn "div" = Div
strToFn "mul" = Mul
strToFn "sub" = Sub
strToFn "ccrgb" = Ccrgb
strToFn "cchsl" = Cchsl
strToFn _ = Unknown

fnToStr :: Fn -> String
fnToStr Num = "num"
fnToStr X = "x"
fnToStr Y = "y"
fnToStr Atan = "atan"
fnToStr Abs = "abs"
fnToStr Cos = "cos"
fnToStr Exp = "exp"
fnToStr Log = "log"
fnToStr Neg = "neg"
fnToStr Rd = "rd"
fnToStr Ru = "ru"
fnToStr Sin = "sin"
fnToStr Add = "add"
fnToStr Div = "div"
fnToStr Mul = "mul"
fnToStr Sub = "sub"
fnToStr Ccrgb = "ccrgb"
fnToStr Cchsl = "cchsl"
fnToStr Unknown = "???"

t1 :: (a, b, c) -> a
t1 (v, _, _) = v
t2 :: (a, b, c) -> b
t2 (_, v, _) = v
t3 :: (a, b, c) -> c
t3 (_, _, v) = v

safeDiv :: Fractional a => a -> a -> a
safeDiv a 0.0 = 0.0
safeDiv a b = a / b

mappingFn :: Mapping -> Double -> Double
mappingFn Clip d = if d > 1.0 then 1.0
                            else if d < -1.0 then -1.0
                                             else d
mappingFn Wrap d = if d == 1.0 then 1.0
                               else (quot - (toDouble $ floor quot)) * 2.0 - 1.0
                   where quot = (d+1.0) / 2.0
mappingFn UnknownMapping d = d

finalHslToRgb :: Double -> Double -> Double -> Double
finalHslToRgb tc q p | tc < (1.0/6.0) = p + ((q-p) * 6.0 * tc)
                     | tc < (1.0/2.0) = q
                     | tc < (2.0/3.0) = p + ((q-p) * ((2.0/3.0) - tc) * 6.0)
                     | otherwise      = p

hslInterval :: Double -> Double
hslInterval t = if t < 0.0 then t + 1.0
                           else if t > 1.0 then t - 1.0
                                           else t

hslToRgb :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
hslToRgb c1 c2 c3 = if (s == 0.0)
                      then (l * 2.0 - 1.0, l * 2.0 - 1.0, l * 2.0 - 1.0)
                      else (cr * 2.0 - 1.0, cg * 2.0 - 1.0, cb * 2.0 - 1.0)
                    where hOrig = 0.3 * t1 c1 + 0.59 * t2 c1 + 0.11 * t3 c1
                          sOrig = 0.3 * t1 c2 + 0.59 * t2 c2 + 0.11 * t3 c2
                          lOrig = 0.3 * t1 c3 + 0.59 * t2 c3 + 0.11 * t3 c3
                          h = (hOrig + 1.0) / 2.0
                          s = (sOrig + 1.0) / 2.0
                          l = (lOrig + 1.0) / 2.0
                          q = (\x -> if (l < 0.5) then (l * (1.0 + s)) else (l + s - (l * s)))(l)
                          p = 2.0 * l - q
                          tr = hslInterval (h + (1.0/3.0))
                          tg = hslInterval h
                          tb = hslInterval (h - (1.0/3.0))
                          cr = finalHslToRgb tr q p
                          cg = finalHslToRgb tg q p
                          cb = finalHslToRgb tb q p

oneEval :: (Double -> Double) -> (Double, Double, Double) -> (Double, Double, Double)
oneEval f (a,b,c) = (f a, f b, f c)

-- type of function, child values, x, y
simpleEval :: Fn -> FullFn -> [(Double, Double, Double)] -> Double -> Double -> (Double, Double, Double)
simpleEval X _ [] x _ = (x, x, x)
simpleEval Y _ [] _ y = (y, y, y)
simpleEval Num obj [] _ _ = let v = valueToDouble (obj M.! "val") in (v,v,v)
simpleEval Atan _ [c] _ _ = oneEval atan c
simpleEval Abs _ [c] _ _ = oneEval abs c
simpleEval Cos _ [c] _ _ = oneEval cos c
simpleEval Exp _ [c] _ _ = oneEval exp c
simpleEval Log _ [c] _ _ = oneEval (\x -> if x <= 0.0 then 0.0 else log x) c
simpleEval Neg _ [c] _ _ = oneEval ((-1) *) c
simpleEval Rd _ [c] _ _ = oneEval (toDouble . floor) c
simpleEval Ru _ [c] _ _ = oneEval (toDouble . ceiling) c
simpleEval Sin _ [c] _ _ = oneEval sin c
simpleEval Add _ [c1, c2] _ _ = (t1 c1 + t1 c2, t2 c1 + t2 c2, t3 c1 + t3 c2)
simpleEval Div _ [c1, c2] _ _ = (safeDiv (t1 c1) (t1 c2), safeDiv (t2 c1) (t2 c2), safeDiv (t3 c1) (t3 c2))
simpleEval Mul _ [c1, c2] _ _ = (t1 c1 * t1 c2, t2 c1 * t2 c2, t3 c1 * t3 c2)
simpleEval Sub _ [c1, c2] _ _ = (t1 c1 - t1 c2, t2 c1 - t2 c2, t3 c1 - t3 c2)
simpleEval Ccrgb _ [c1, c2, c3] _ _ = (t1 c1, t2 c2, t3 c3)
simpleEval Cchsl _ [c1, c2, c3] _ _ = hslToRgb c1 c2 c3
simpleEval _ _ _ _ _ = (0.0, 0.0, 0.0)

evalAtPixel :: (Double, Double) -> FullFn -> (Double, Double, Double)
evalAtPixel (x,y) fullfn = simpleEval (getFnFromValue (fullfn M.! "t")) fullfn (map (evalAtPixel (x,y)) (getChildrenFromValue (M.findWithDefault Null "ch" fullfn))) x y

getChildrenFromValue :: Value -> [FullFn]
getChildrenFromValue (Array vs) = map getFullFnFromValue vs
getChildrenFromValue _ = []

getFullFnFromValue :: Value -> FullFn
getFullFnFromValue (Object fn) = fn

getFnFromValue :: Value -> Fn
getFnFromValue (String s) = read s

valueToDouble :: Value -> Double
valueToDouble (Number n) = n
valueToDouble _ = 0.0 -- TODO - error?

-- Taken from http://haskell.org/ghc/docs/6.0.1/html/users_guide/pragmas.html
toDouble :: Real a => a -> Double
toDouble = fromRational . toRational

--{-# RULES "toDouble/Int" toDouble = i2d #-}
--i2d (I# i) = D# (int2Double# i) -- uses Glasgow prim-op directly

parseIncomingJSON :: String -> Maybe Value
parseIncomingJSON s = parse $ map singleQuoteToDouble s

singleQuoteToDouble :: Char -> Char
singleQuoteToDouble '\'' = '"'
singleQuoteToDouble c = c

getPoints :: (Fractional a, Enum a, Fractional b, Enum b) => a -> b -> [[(a, b)]]
getPoints width height = [[(x,y) | x <- xs] | y <- ys]
                         where xs = [-1.0,-1.0+(2.0/width) .. 1.0]
                               ys = [1.0, 1.0-(2.0/height) .. -1.0]

evalFunction :: Int -> Int -> Maybe Value -> [[(Double, Double, Double)]]
evalFunction _ _ Nothing = [[]]
evalFunction width height (Just v) = [map (\pt -> evalAtPixel pt (getFullFnFromValue v)) x | x <- getPoints (toDouble width) (toDouble height)]
