-- | This module contains fuctions and templates for building up and breaking
--   down packed bit structures. It's something like Erlang's bit-syntax (or,
--   actually, more like Python's struct module).
--
--   This code uses Data.ByteString which is included in GHC 6.5 and you can
--   get it for 6.4 at <http://www.cse.unsw.edu.au/~dons/fps.html>
module BitSyntax (
  -- * Building bit structures
  -- | The core function here is makeBits, which is a perfectly normal function.
  --   Here's an example which makes a SOCKS4a request header:
  -- @
  --   makeBits [U8 4, U8 1, U16 80, U32 10, NullTerminated \"username\",
  --             NullTerminated \"www.haskell.org\"]
  -- @
  BitBlock(..),
  makeBits,
  -- * Breaking up bit structures
  -- | The main function for this is bitSyn, which is a template function and
  --   so you'll need to run with @-fth@ to enable template haskell
  --   <http://www.haskell.org/th/>. This function expands at the place where its
  --   used and includes references to functions by name, so those references need
  --   to resolve at the point of /use/. To make sure that happens you'll need:
  --
  -- > import BitSyntax
  -- > import qualified Data.ByteString as BS
  --
  --   To expand the function you use the splice command:
  -- @
  --   $(bitSyn [...])
  -- @
  --
  -- The expanded function has type @ByteString -> (...)@ where the elements of
  -- the tuple depend of the argument to bitSyn (that's why it has to be a template
  -- function).
  --
  -- Heres an example, translated from the Erlang manual, which parses an IP header:
  --
  -- @
  -- decodeOptions bs ([_, hlen], _, _, _, _, _, _, _, _, _) =
  --   if hlen > 5
  --     then BS.splitAt (fromIntegral ((hlen - 5) * 4)) bs
  --     else (BS.empty, bs)
  -- @
  --
  -- @
  -- ipDecode = $(bitSyn [PackedBits [4, 4], Unsigned 1, Unsigned 2, Unsigned 2,
  --                      PackedBits [3, 13], Unsigned 1, Unsigned 1, Unsigned 2,
  --                      Fixed 4, Fixed 4, Context \"decodeOptions\", Rest])
  -- @
  -- 
  -- @
  -- ipPacket = BS.pack [0x45, 0, 0, 0x34, 0xd8, 0xd2, 0x40, 0, 0x40, 0x06,
  --                     0xa0, 0xca, 0xac, 0x12, 0x68, 0x4d, 0xac, 0x18,
  --                     0x00, 0xaf]
  -- @
  --
  -- This function has several weaknesses compared to the Erlang version: The
  -- elements of the bit structure are not named in place, instead you have to
  -- do a pattern match on the resulting tuple and match up the indexes. The
  -- type system helps in this, but it's still not quite as nice.
  --
  -- The need to have the correct functions in scope (as pointed out above) is a
  -- problem.

  ReadType(..), bitSyn,
  -- * Utilitiy functions
  -- | These are exposed because bitSyn is a template function and so
  --   functions referred to by it have to be in scope at the location where
  --   bitSyn is used.
  decodeU8, decodeU16, decodeU32, decodeBits) where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Char (chr, ord)
import Control.Monad
import Test.QuickCheck

import Foreign
import Foreign.C

foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32
foreign import ccall unsafe "htons" htons :: Word16 -> Word16

data BitBlock = -- | Unsigned 8-bit int
                U8 Int |
                -- | Unsigned 16-bit int
                U16 Int |
                -- | Unsigned 32-bit int
                U32 Int |
                -- | Appends the string with a trailing NUL byte
                NullTerminated String |
                -- | Appends the string without any terminator
                RawString String |
                -- | Appends a ByteString
                RawByteString BS.ByteString |
                -- | Packs a series of bit fields together. The argument is
                --   a list of pairs where the first element is the size
                --   (in bits) and the second is the value. The sum of the
                --   sizes for a given PackBits must be a multiple of 8
                PackBits [(Int, Int)]
                deriving (Show)

-- Encodes a member of the Bits class as a series of bytes and returns the
-- ByteString of those bytes.
getBytes :: (Integral a, Bounded a, Bits a) => a -> BS.ByteString
getBytes input =
    let getByte _ 0 = []
        getByte x remaining = (fromIntegral $ (x .&. 0xff)) :
                              getByte (shiftR x 8) (remaining - 1)
        in
        if (bitSize input `mod` 8) /= 0
           then error "Input data bit size must be a multiple of 8"
           else BS.pack $ getByte input (bitSize input `div` 8)

-- Performs the work behind PackBits
packBits :: (Word8, Int, [Word8])  -- ^ The current byte, the number of bits
                                   --   used in that byte and the (reverse)
                                   --   list of produced bytes
         -> (Int, Int)  -- ^ The size (in bits) of the value, and the value
         -> (Word8, Int, [Word8])  -- See first argument
packBits (current, used, bytes) (size, value) =
  if bitsWritten < size
    then packBits (0, 0, current' : bytes) (size - bitsWritten, value)
    else if used' == 8
           then (0, 0, current' : bytes)
           else (current', used', bytes)
  where
    top = size - 1
    topOfByte = 7 - used
    aligned = value `shift` (topOfByte - top)
    newBits = (fromIntegral aligned) :: Word8
    current' = current .|. newBits
    bitsWritten = min (8 - used) size
    used' = used + bitsWritten

bits (U8 v) = BS.pack [((fromIntegral v) :: Word8)]
bits (U16 v) = getBytes ((htons $ fromIntegral v) :: Word16)
bits (U32 v) = getBytes ((htonl $ fromIntegral v) :: Word32)
bits (NullTerminated str) = BS.pack $ (map (fromIntegral . ord) str) ++ [0]
bits (RawString str) = BS.pack $ map (fromIntegral . ord) str
bits (RawByteString bs) = bs
bits (PackBits bitspec) =
  if (sum $ map fst bitspec) `mod` 8 /= 0
    then error "Sum of sizes of a bit spec must == 0 mod 8"
    else (\(_, _, a) -> BS.pack $ reverse a) $ foldl packBits (0, 0, []) bitspec

-- | Make a binary string from the list of elements given
makeBits :: [BitBlock] -> BS.ByteString
makeBits = BS.concat . (map bits)

data ReadType = -- | An unsigned number of some number of bytes. Valid
                --   arguments are 1, 2 and 4
                Unsigned Integer |
                -- | A variable length element to be decoded by a custom
                --   function. The function's name is given as the single
                --   argument and should have type
                --   @ByteString -> (v, ByteString)@
                Variable String |
                -- | Skip some number of bytes
                Skip Integer |
                -- | A fixed size field, the result of which is a ByteString
                --   of that length.
                Fixed Integer |
                -- | Decode a value and ignore it (the result will not be part
                --   of the returned tuple)
                Ignore ReadType |
                -- | Like variable, but the decoding function is passed the
                --   entire result tuple so far. Thus the function whose name
                --   passed has type @ByteString -> (...) -> (v, ByteString)@
                Context String |
                -- | Takes the most recent element of the result tuple and
                --   interprets it as the length of this field. Results in
                --   a ByteString
                LengthPrefixed |
                -- | Decode a series of bit fields, results in a list of
                --   Integers. Each element of the argument is the length of
                --   the bit field. The sums of the lengths must be a multiple
                --   of 8
                PackedBits [Integer] |
                -- | Results in a ByteString containing the undecoded bytes so
                --   far. Generally used at the end to return the trailing body
                --   of a structure, it can actually be used at any point in the
                --   decoding to return the trailing part at that point.
                Rest

fromBytes :: (Bits a) => [a] -> a
fromBytes input =
    let dofb accum [] = accum
        dofb accum (x:xs) = dofb ((shiftL accum 8) .|. x) xs
        in
        dofb 0 $ reverse input

decodeU8 :: BS.ByteString -> Word8
decodeU8 = fromIntegral . head . BS.unpack
decodeU16 :: BS.ByteString -> Word16
decodeU16 = htons . fromBytes . map fromIntegral . BS.unpack
decodeU32 :: BS.ByteString -> Word32
decodeU32 = htonl . fromBytes . map fromIntegral . BS.unpack

decodeBits :: [Integer] -> BS.ByteString -> [Integer]
decodeBits sizes bs =
  reverse values
  where
    (values, _, _) = foldl unpackBits ([], 0, BS.unpack bitdata) sizes
    bytesize = (sum sizes) `shiftR` 3
    (bitdata, rest) = BS.splitAt (fromIntegral bytesize) bs

unpackBits :: ([Integer], Integer, [Word8]) -> Integer -> ([Integer], Integer, [Word8])
unpackBits state size = unpackBitsInner 0 state size

unpackBitsInner :: Integer ->
                   ([Integer], Integer, [Word8]) ->
                   Integer ->
                   ([Integer], Integer, [Word8])
unpackBitsInner _ (output, used, []) _ = (output, used, [])
unpackBitsInner val (output, used, current : input) bitsToGet =
  if bitsToGet' > 0
    then unpackBitsInner val'' (output, 0, input) bitsToGet'
    else if used' < 8
           then (val'' : output, used', current'' : input)
           else (val'' : output, 0, input)
  where
    bitsAv = 8 - used
    bitsTaken = min bitsAv bitsToGet
    val' = val `shift` (fromIntegral bitsTaken)
    current' = current `shiftR` (fromIntegral (8 - bitsTaken))
    current'' = current `shiftL` (fromIntegral bitsTaken)
    val'' = val' .|. (fromIntegral current')
    bitsToGet' = bitsToGet - bitsTaken
    used' = used + bitsTaken

readElement :: ([Dec], Name, [Name]) -> ReadType -> Q ([Dec], Name, [Name])

readElement (decs, inputname, tuplenames) (Context funcname) = do
  valname <- newName "val"
  restname <- newName "rest"

  let dec = ValD (TupP [VarP valname, VarP restname])
                 (NormalB $ AppE (AppE (VarE $ mkName funcname)
                                       (VarE inputname))
                                 (TupE $ map VarE $ reverse tuplenames))
                 []
  return (dec : decs, restname, valname : tuplenames)

readElement (decs, inputname, tuplenames) (Fixed n) = do
  valname <- newName "val"
  restname <- newName "rest"
  let dec1 = ValD (TupP [VarP valname, VarP restname])
                  (NormalB $ AppE (AppE (VarE $ mkName "BS.splitAt")
                                        (LitE (IntegerL n)))
                                  (VarE inputname))
                  []

  return (dec1 : decs, restname, valname : tuplenames)

readElement state@(_, _, tuplenames) (Ignore n) = do
  (a, b, c) <- readElement state n
  return (a, b, tuplenames)

readElement (decs, inputname, tuplenames) LengthPrefixed = do
  valname <- newName "val"
  restname <- newName "rest"

  let sourcename = head tuplenames
      dec = ValD (TupP [VarP valname, VarP restname])
                 (NormalB $ AppE (AppE (VarE $ mkName "BS.splitAt")
                                       (AppE (VarE $ mkName "fromIntegral")
                                             (VarE sourcename)))
                                 (VarE inputname))
                 []

  return (dec : decs, restname, valname : tuplenames)

readElement (decs, inputname, tuplenames) (Variable funcname) = do
  valname <- newName "val"
  restname <- newName "rest"

  let dec = ValD (TupP [VarP valname, VarP restname])
                 (NormalB $ AppE (VarE $ mkName funcname)
                                 (VarE inputname))
                 []
  return (dec : decs, restname, valname : tuplenames)

readElement (decs, inputname, tuplenames) Rest = do
  restname <- newName "rest"
  let dec = ValD (VarP restname)
                 (NormalB $ VarE inputname)
                 []
  return (dec : decs, inputname, restname : tuplenames)

readElement (decs, inputname, tuplenames) (Skip n) = do
  -- Expands to something like:
  --   rest = BS.drop n input
  restname <- newName "rest"
  let dec = ValD (VarP restname)
                 (NormalB $ AppE (AppE (VarE $ mkName "BS.drop")
                                       (LitE (IntegerL n)))
                                 (VarE inputname))
                 []
  return (dec : decs, restname, tuplenames)

readElement state (Unsigned size) = do
  -- Expands to something like:
  --    (aval, arest) = BS.splitAt 1 input
  --    a = decodeU8 aval
  let decodefunc = case size of
                     1 -> "decodeU8"
                     2 -> "decodeU16"
                     4 -> "decodeU32"
  decodeHelper state (VarE $ mkName decodefunc) size

readElement state (PackedBits sizes) =
  if sum sizes `mod` 8 /= 0
    then error "Sizes of packed bits must == 0 mod 8"
    else decodeHelper state
                      (AppE (VarE $ mkName "decodeBits")
                            (ListE $ map (LitE . IntegerL) sizes))
                      ((sum sizes) `shiftR` 3)

decodeHelper (decs, inputname, tuplenames) decodefunc size = do
  valname <- newName "val"
  restname <- newName "rest"
  tuplename <- newName "tup"
  let dec1 = ValD (TupP [VarP valname, VarP restname])
                  (NormalB $ AppE (AppE (VarE $ mkName "BS.splitAt")
                                        (LitE (IntegerL size)))
                                  (VarE inputname))
                  []
  let dec2 = ValD (VarP tuplename)
                  (NormalB $ AppE decodefunc (VarE valname))
                  []

  return (dec1 : dec2 : decs, restname, tuplename : tuplenames)

decGetName (ValD (VarP name) _ _) = name

bitSyn :: [ReadType] -> Q Exp
bitSyn elements = do
    inputname <- newName "input"
    (lets, restname, tuplenames) <- foldM readElement ([], inputname, []) elements
    return $ LamE [VarP inputname] (LetE lets $ TupE $ map VarE $ reverse tuplenames)


-- Tests

prop_bitPacking fields =
  prevalues == (map fromIntegral postvalues) ||
  any (< 1) (map fst fields) ||
  any (< 0) (map snd fields)
  where
    undershoot = sum (map fst fields) `mod` 8
    fields' = if undershoot > 0
                then (8 - undershoot, 1) : fields
                else fields
    prevalues = map snd fields'
    packed = bits $ PackBits fields'
    postvalues = decodeBits (map (fromIntegral . fst) fields') packed
