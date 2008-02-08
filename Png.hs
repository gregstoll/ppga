{-
A small library for creating monochrome PNG files.
This file is placed into the public domain.
Dependencies: Zlib.
-}
module Png (png, colorPng) where
import Data.Array
import Data.Bits
import Data.List
import Data.Word
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as B
 
be8 :: Word8 -> B.ByteString
be8 x = B.singleton x
 
be32 :: Word32 -> B.ByteString
be32 x = B.pack [fromIntegral (x `shiftR` sh) | sh <- [24,16,8,0]]
 
pack :: String -> B.ByteString
pack xs = B.pack $ map (fromIntegral.fromEnum) xs
 
unpack :: B.ByteString -> String
unpack xs = map (toEnum.fromIntegral) (B.unpack xs)
 
hdr, iHDR, iDAT, iEND :: B.ByteString
hdr = pack "\137\80\78\71\13\10\26\10"
iHDR = pack "IHDR"
iDAT = pack "IDAT"
iEND = pack "IEND"
 
chunk :: B.ByteString -> B.ByteString -> [B.ByteString]
chunk tag xs = [be32 (fromIntegral $ B.length xs), dat, be32 (crc dat)]
    where dat = B.append tag xs
 
-- | Return a monochrome PNG file from a two dimensional bitmap
-- stored in a list of lines represented as a list of booleans.
png :: [[Bool]] -> String
png dat = unpack $ B.concat $ hdr : concat [ihdr, imgdat, iend]
    where height = fromIntegral $ length dat
          width = fromIntegral $ length (head dat)
          ihdr = chunk iHDR (B.concat [
                be32 width, be32 height, be8 1, be8 0, be8 0, be8 0, be8 0])
          imgdat = chunk iDAT (Z.compress imgbits)
          imgbits = B.concat $ map scanline dat
          iend = chunk iEND B.empty

colorPng :: [[(Word8, Word8, Word8)]] -> String
colorPng dat = unpack $ B.concat $ hdr : concat [ihdr, imgdat, iend]
    where height = fromIntegral $ length dat
          width = fromIntegral $ length (head dat)
          ihdr = chunk iHDR (B.concat [
                be32 width, be32 height, be8 8, be8 2, be8 0, be8 0, be8 0])
          imgdat = chunk iDAT (Z.compress imgbits)
          imgbits = B.concat $ map colorScanline dat
          iend = chunk iEND B.empty

scanline :: [Bool] -> B.ByteString
scanline dat = 0 `B.cons` bitpack dat

colorScanline :: [(Word8, Word8, Word8)] -> B.ByteString
colorScanline dat = 0 `B.cons` colorBitpack dat

t1 :: (a, b, c) -> a
t1 (x, _, _) = x
t2 :: (a, b, c) -> b
t2 (_, y, _) = y
t3 :: (a, b, c) -> c
t3 (_, _, z) = z

bitpack' :: [Bool] -> Word8 -> Word8 -> B.ByteString
bitpack' [] n b = if b /= 0x80 then B.singleton n else B.empty
bitpack' (x:xs) n b =
    if b == 1
        then v `B.cons` bitpack' xs 0 0x80
        else bitpack' xs v (b `shiftR` 1)
    where v = if x then n else n .|. b
 
--colorBitpack' :: [(Word8, Word8, Word8)] -> B.ByteString
--colorBitpack' [] = B.empty
--colorBitpack' (x:xs) =
--        map B.pack([(t1 x), (t2 x), (t3 x)]) (colorBitpack' xs)
 
bitpack :: [Bool] -> B.ByteString
bitpack xs = bitpack' xs 0 0x80
 
colorBitpack :: [(Word8, Word8, Word8)] -> B.ByteString
--colorBitpack xs = B.fromChunks (map (\x -> B.pack [(t1 x), t2 x, t3 x]) xs)
colorBitpack xs = B.reverse (foldl (\accum y -> (B.cons (t3 y) (B.cons (t2 y) (B.cons (t1 y) accum)))) B.empty xs)

crc :: B.ByteString -> Word32
crc xs = updateCrc 0xffffffff xs `xor` 0xffffffff
 
updateCrc :: Word32 -> B.ByteString -> Word32
updateCrc = B.foldl' crcStep
 
crcStep :: Word32 -> Word8 -> Word32
crcStep crc ch = (crcTab ! n) `xor` (crc `shiftR` 8)
    where n = fromIntegral (crc `xor` fromIntegral ch)
 
crcTab :: Array Word8 Word32
crcTab = array (0,255) $ zip [0..255] $ flip map [0..255] (\n ->
    foldl' (\c k -> if c .&. 1 == 1
                      then 0xedb88320 `xor` (c `shiftR` 1)
                      else c `shiftR` 1) n [0..7])
