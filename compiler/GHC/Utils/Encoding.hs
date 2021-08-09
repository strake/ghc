{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 1997-2006
--
-- Character encodings
--
-- -----------------------------------------------------------------------------

module GHC.Utils.Encoding (
        -- * UTF-8
        utf8DecodeCharAddr#,
        utf8PrevChar,
        utf8CharStart,
        utf8DecodeChar,
        utf8DecodeByteString,
        utf8DecodeShortByteString,
        utf8DecodeStringLazy,
        utf8EncodeChar,
        utf8EncodeString,
        utf8EncodeShortByteString,
        utf8EncodedLength,
        countUTF8Chars,

        -- * Z-encoding
        zEncodeString,
        zDecodeString,

        -- * Base62-encoding
        toBase62,
        toBase62Padded
  ) where

import GHC.Prelude

import Foreign
import Data.Char
import qualified Data.Char as Char
import Numeric
import GHC.IO
import GHC.ST

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.ByteString.Short.Internal (ShortByteString(..))

import GHC.Exts

-- -----------------------------------------------------------------------------
-- UTF-8

-- We can't write the decoder as efficiently as we'd like without
-- resorting to unboxed extensions, unfortunately.  I tried to write
-- an IO version of this function, but GHC can't eliminate boxed
-- results from an IO-returning function.
--
-- We assume we can ignore overflow when parsing a multibyte character here.
-- To make this safe, we add extra sentinel bytes to unparsed UTF-8 sequences
-- before decoding them (see "GHC.Data.StringBuffer").

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: (Int# -> Word#) -> (# Char#, Int# #)
utf8DecodeChar# indexWord8# =
  let !ch0 = word2Int# (indexWord8# 0#) in
  case () of
    _ | isTrue# (ch0 <=# 0x7F#) -> (# chr# ch0, 1# #)

      | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
        let !ch1 = word2Int# (indexWord8# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        (# chr# (((ch0 -# 0xC0#) `uncheckedIShiftL#` 6#) +#
                  (ch1 -# 0x80#)),
           2# #)

      | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
        let !ch1 = word2Int# (indexWord8# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        let !ch2 = word2Int# (indexWord8# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then fail 2# else
        (# chr# (((ch0 -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch2 -# 0x80#)),
           3# #)

     | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
        let !ch1 = word2Int# (indexWord8# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        let !ch2 = word2Int# (indexWord8# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then fail 2# else
        let !ch3 = word2Int# (indexWord8# 3#) in
        if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then fail 3# else
        (# chr# (((ch0 -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 12#) +#
                 ((ch2 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch3 -# 0x80#)),
           4# #)

      | otherwise -> fail 1#
  where
        -- all invalid sequences end up here:
        fail :: Int# -> (# Char#, Int# #)
        fail nBytes# = (# '\0'#, nBytes# #)
        -- '\xFFFD' would be the usual replacement character, but
        -- that's a valid symbol in Haskell, so will result in a
        -- confusing parse error later on.  Instead we use '\0' which
        -- will signal a lexer error immediately.

utf8DecodeCharAddr# :: Addr# -> (# Char#, Int# #)
utf8DecodeCharAddr# a# =
    utf8DecodeChar# (indexWord8OffAddr# a#)

utf8DecodeCharByteArray# :: ByteArray# -> Int# -> (# Char#, Int# #)
utf8DecodeCharByteArray# ba# off# =
    utf8DecodeChar# (\i# -> indexWord8Array# ba# (i# +# off#))

utf8DecodeChar :: Ptr Word8 -> (Char, Int)
utf8DecodeChar !(Ptr a#) =
  case utf8DecodeCharAddr# a# of
    (# c#, nBytes# #) -> ( C# c#, I# nBytes# )

-- UTF-8 is cleverly designed so that we can always figure out where
-- the start of the current character is, given any position in a
-- stream.  This function finds the start of the previous character,
-- assuming there *is* a previous character.
utf8PrevChar :: Ptr Word8 -> IO (Ptr Word8)
utf8PrevChar p = utf8CharStart (p `plusPtr` (-1))

utf8CharStart :: Ptr Word8 -> IO (Ptr Word8)
utf8CharStart p = go p
 where go p = do w <- peek p
                 if w >= 0x80 && w < 0xC0
                        then go (p `plusPtr` (-1))
                        else return p

{-# INLINE utf8DecodeLazy# #-}
utf8DecodeLazy# :: (IO ()) -> (Int# -> Word#) -> Int# -> IO [Char]
utf8DecodeLazy# retain indexWord8# len#
  = unpack 0#
  where
    unpack i#
        | isTrue# (i# >=# len#) = retain >> return []
        | otherwise =
            case utf8DecodeChar# (\j# -> indexWord8# (i# +# j#)) of
              (# c#, nBytes# #) -> do
                rest <- unsafeDupableInterleaveIO $ unpack (i# +# nBytes#)
                return (C# c# : rest)

utf8DecodeByteString :: ByteString -> [Char]
utf8DecodeByteString (BS.PS fptr offset len)
  = utf8DecodeStringLazy fptr offset len

utf8DecodeStringLazy :: ForeignPtr Word8 -> Int -> Int -> [Char]
utf8DecodeStringLazy fp offset (I# len#)
  = unsafeDupablePerformIO $ withForeignPtr fp $ \ptr ->
      let !(Ptr a#) = ptr `plusPtr` offset
          index# = indexWord8OffAddr# a# in
      utf8DecodeLazy# (touchForeignPtr fp) index# len#

utf8DecodeShortByteString :: ShortByteString -> [Char]
utf8DecodeShortByteString (SBS ba#)
  = unsafeDupablePerformIO $
      let index# = indexWord8Array# ba#
          len# = sizeofByteArray# ba# in
      utf8DecodeLazy# (return ()) index# len#

countUTF8Chars :: ShortByteString -> IO Int
countUTF8Chars (SBS ba) = go 0# 0#
  where
    len# = sizeofByteArray# ba
    go i# n#
      | isTrue# (i# >=# len#) =
          return (I# n#)
      | otherwise = do
          case utf8DecodeCharByteArray# ba i# of
            (# _, nBytes# #) -> go (i# +# nBytes#) (n# +# 1#)

{-# INLINE utf8EncodeChar #-}
utf8EncodeChar :: (Int# -> Word# -> State# s -> State# s)
               -> Char -> ST s Int
utf8EncodeChar write# c =
  let x = ord c in
  case () of
    _ | x > 0 && x <= 0x007f -> do
          write 0 x
          return 1
        -- NB. '\0' is encoded as '\xC0\x80', not '\0'.  This is so that we
        -- can have 0-terminated UTF-8 strings (see GHC.Base.unpackCStringUtf8).
      | x <= 0x07ff -> do
          write 0 (0xC0 .|. ((x `shiftR` 6) .&. 0x1F))
          write 1 (0x80 .|. (x .&. 0x3F))
          return 2
      | x <= 0xffff -> do
          write 0 (0xE0 .|. (x `shiftR` 12) .&. 0x0F)
          write 1 (0x80 .|. (x `shiftR` 6) .&. 0x3F)
          write 2 (0x80 .|. (x .&. 0x3F))
          return 3
      | otherwise -> do
          write 0 (0xF0 .|. (x `shiftR` 18))
          write 1 (0x80 .|. ((x `shiftR` 12) .&. 0x3F))
          write 2 (0x80 .|. ((x `shiftR` 6) .&. 0x3F))
          write 3 (0x80 .|. (x .&. 0x3F))
          return 4
  where
    {-# INLINE write #-}
    write (I# off#) (I# c#) = ST $ \s ->
      case write# off# (int2Word# c#) s of
        s -> (# s, () #)

utf8EncodeString :: Ptr Word8 -> String -> IO ()
utf8EncodeString (Ptr a#) str = go a# str
  where go !_   []   = return ()
        go a# (c:cs) = do
          I# off# <- stToIO $ utf8EncodeChar (writeWord8OffAddr# a#) c
          go (a# `plusAddr#` off#) cs

utf8EncodeShortByteString :: String -> IO ShortByteString
utf8EncodeShortByteString str = stToIO $ ST $ \s ->
  let !(I# len#) = utf8EncodedLength str in
  case newByteArray# len# s of { (# s, mba# #) ->
  let ST f_go = go mba# 0# str in
  case f_go s of { (# s, () #) ->
  case unsafeFreezeByteArray# mba# s of { (# s, ba# #) ->
  (# s, SBS ba# #) }}}
  where
    go _ _ [] = return ()
    go mba# i# (c:cs) = do
      I# off# <- utf8EncodeChar (\j# -> writeWord8Array# mba# (i# +# j#)) c
      go mba# (i# +# off#) cs

utf8EncodedLength :: String -> Int
utf8EncodedLength str = go 0 str
  where go !n [] = n
        go n (c:cs)
          | ord c > 0 && ord c <= 0x007f = go (n+1) cs
          | ord c <= 0x07ff = go (n+2) cs
          | ord c <= 0xffff = go (n+3) cs
          | otherwise       = go (n+4) cs

-- -----------------------------------------------------------------------------
-- The Z-encoding

{-
This is the main name-encoding and decoding function.  It encodes any
string into a string that is acceptable as a C name.  This is done
right before we emit a symbol name into the compiled C or asm code.
Z-encoding of strings is cached in the FastString interface, so we
never encode the same string more than once.

The basic encoding scheme is this.

* Tuples (,,,) are coded as Z3T

* Alphabetic characters (upper and lower) and digits
        all translate to themselves;
        except 'Z', which translates to 'ZZ'
        and    'z', which translates to 'zz'
  We need both so that we can preserve the variable/tycon distinction

* Most other printable characters translate to 'zx' or 'Zx' for some
        alphabetic character x

* The others translate as 'znnnU' where 'nnn' is the decimal number
        of the character

        Before          After
        --------------------------
        Trak            Trak
        foo_wib         foozuwib
        >               zg
        >1              zg1
        foo#            foozh
        foo##           foozhzh
        foo##1          foozhzh1
        fooZ            fooZZ
        :+              ZCzp
        ()              Z0T     0-tuple
        (,,,,)          Z5T     5-tuple
        (# #)           Z1H     unboxed 1-tuple (note the space)
        (#,,,,#)        Z5H     unboxed 5-tuple
                (NB: There is no Z1T nor Z0H.)
-}

type UserString = String        -- As the user typed it
type EncodedString = String     -- Encoded form


zEncodeString :: UserString -> EncodedString
zEncodeString cs = case maybe_tuple cs of
                Just n  -> n            -- Tuples go to Z2T etc
                Nothing -> go cs
          where
                go []     = []
                go (c:cs) = encode_digit_ch c ++ go' cs
                go' []     = []
                go' (c:cs) = encode_ch c ++ go' cs

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

-- If a digit is at the start of a symbol then we need to encode it.
-- Otherwise package names like 9pH-0.1 give linker errors.
encode_digit_ch :: Char -> EncodedString
encode_digit_ch c | c >= '0' && c <= '9' = encode_as_unicode_char c
encode_digit_ch c | otherwise            = encode_ch c

encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]     -- Common case first

-- Constructors
encode_ch '('  = "ZL"   -- Needed for things like (,), and (->)
encode_ch ')'  = "ZR"   -- For symmetry with (
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"

-- Variables
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = encode_as_unicode_char c

encode_as_unicode_char :: Char -> EncodedString
encode_as_unicode_char c = 'z' : if isDigit (head hex_str) then hex_str
                                                           else '0':hex_str
  where hex_str = showHex (ord c) "U"
  -- ToDo: we could improve the encoding here in various ways.
  -- eg. strings of unicode characters come out as 'z1234Uz5678U', we
  -- could remove the 'U' in the middle (the 'z' works as a separator).

zDecodeString :: EncodedString -> UserString
zDecodeString [] = []
zDecodeString ('Z' : d : rest)
  | isDigit d = decode_tuple   d rest
  | otherwise = decode_upper   d : zDecodeString rest
zDecodeString ('z' : d : rest)
  | isDigit d = decode_num_esc d rest
  | otherwise = decode_lower   d : zDecodeString rest
zDecodeString (c   : rest) = c : zDecodeString rest

decode_upper, decode_lower :: Char -> Char

decode_upper 'L' = '('
decode_upper 'R' = ')'
decode_upper 'M' = '['
decode_upper 'N' = ']'
decode_upper 'C' = ':'
decode_upper 'Z' = 'Z'
decode_upper ch  = {-pprTrace "decode_upper" (char ch)-} ch

decode_lower 'z' = 'z'
decode_lower 'a' = '&'
decode_lower 'b' = '|'
decode_lower 'c' = '^'
decode_lower 'd' = '$'
decode_lower 'e' = '='
decode_lower 'g' = '>'
decode_lower 'h' = '#'
decode_lower 'i' = '.'
decode_lower 'l' = '<'
decode_lower 'm' = '-'
decode_lower 'n' = '!'
decode_lower 'p' = '+'
decode_lower 'q' = '\''
decode_lower 'r' = '\\'
decode_lower 's' = '/'
decode_lower 't' = '*'
decode_lower 'u' = '_'
decode_lower 'v' = '%'
decode_lower ch  = {-pprTrace "decode_lower" (char ch)-} ch

-- Characters not having a specific code are coded as z224U (in hex)
decode_num_esc :: Char -> EncodedString -> UserString
decode_num_esc d rest
  = go (digitToInt d) rest
  where
    go n (c : rest) | isHexDigit c = go (16*n + digitToInt c) rest
    go n ('U' : rest)           = chr n : zDecodeString rest
    go n other = error ("decode_num_esc: " ++ show n ++  ' ':other)

decode_tuple :: Char -> EncodedString -> UserString
decode_tuple d rest
  = go (digitToInt d) rest
  where
        -- NB. recurse back to zDecodeString after decoding the tuple, because
        -- the tuple might be embedded in a longer name.
    go n (c : rest) | isDigit c = go (10*n + digitToInt c) rest
    go 0 ('T':rest)     = "()" ++ zDecodeString rest
    go n ('T':rest)     = '(' : replicate (n-1) ',' ++ ")" ++ zDecodeString rest
    go 1 ('H':rest)     = "(# #)" ++ zDecodeString rest
    go n ('H':rest)     = '(' : '#' : replicate (n-1) ',' ++ "#)" ++ zDecodeString rest
    go n other = error ("decode_tuple: " ++ show n ++ ' ':other)

{-
Tuples are encoded as
        Z3T or Z3H
for 3-tuples or unboxed 3-tuples respectively.  No other encoding starts
        Z<digit>

* "(# #)" is the tycon for an unboxed 1-tuple (not 0-tuple)
  There are no unboxed 0-tuples.

* "()" is the tycon for a boxed 0-tuple.
  There are no boxed 1-tuples.
-}

maybe_tuple :: UserString -> Maybe EncodedString

maybe_tuple "(# #)" = Just("Z1H")
maybe_tuple ('(' : '#' : cs) = case count_commas (0::Int) cs of
                                 (n, '#' : ')' : _) -> Just ('Z' : shows (n+1) "H")
                                 _                  -> Nothing
maybe_tuple "()" = Just("Z0T")
maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
                                 (n, ')' : _) -> Just ('Z' : shows (n+1) "T")
                                 _            -> Nothing
maybe_tuple _                = Nothing

count_commas :: Int -> String -> (Int, String)
count_commas n (',' : cs) = count_commas (n+1) cs
count_commas n cs         = (n,cs)


{-
************************************************************************
*                                                                      *
                        Base 62
*                                                                      *
************************************************************************

Note [Base 62 encoding 128-bit integers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of base-62 encoding a single 128-bit integer
(ceil(21.49) characters), we'll base-62 a pair of 64-bit integers
(2 * ceil(10.75) characters).  Luckily for us, it's the same number of
characters!
-}

--------------------------------------------------------------------------
-- Base 62

-- The base-62 code is based off of 'locators'
-- ((c) Operational Dynamics Consulting, BSD3 licensed)

-- | Size of a 64-bit word when written as a base-62 string
word64Base62Len :: Int
word64Base62Len = 11

-- | Converts a 64-bit word into a base-62 string
toBase62Padded :: Word64 -> String
toBase62Padded w = pad ++ str
  where
    pad = replicate len '0'
    len = word64Base62Len - length str -- 11 == ceil(64 / lg 62)
    str = toBase62 w

toBase62 :: Word64 -> String
toBase62 w = showIntAtBase 62 represent w ""
  where
    represent :: Int -> Char
    represent x
        | x < 10 = Char.chr (48 + x)
        | x < 36 = Char.chr (65 + x - 10)
        | x < 62 = Char.chr (97 + x - 36)
        | otherwise = error "represent (base 62): impossible!"
