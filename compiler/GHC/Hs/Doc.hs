{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Hs.Doc
  ( HsDocString
  , LHsDocString
  , mkHsDocString
  , mkHsDocStringUtf8ByteString
  , unpackHDS
  , hsDocStringToByteString

  , appendDocs
  , concatDocs

  , DeclDocMap(..)
  , emptyDeclDocMap

  , ArgDocMap(..)
  , emptyArgDocMap
  ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Utils.Encoding
import GHC.Utils.IO.Unsafe
import GHC.Types.Name
import GHC.Utils.Outputable as Outputable
import GHC.Types.SrcLoc

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BS
import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign

-- | Haskell Documentation String
--
-- Internally this is a UTF8-Encoded 'ByteString'.
newtype HsDocString = HsDocString ByteString
  -- There are at least two plausible Semigroup instances for this type:
  --
  -- 1. Simple string concatenation.
  -- 2. Concatenation as documentation paragraphs with newlines in between.
  --
  -- To avoid confusion, we pass on defining an instance at all.
  deriving (Eq, Show, Data)
  deriving Binary via ByteString

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr = doubleQuotes . text . unpackHDS

mkHsDocString :: String -> HsDocString
mkHsDocString s =
  inlinePerformIO $ do
    let len = utf8EncodedLength s
    buf <- mallocForeignPtrBytes len
    withForeignPtr buf $ \ptr ->
      HsDocString (BS.fromForeignPtr buf 0 len) <$ utf8EncodeString ptr s

-- | Create a 'HsDocString' from a UTF8-encoded 'ByteString'.
mkHsDocStringUtf8ByteString :: ByteString -> HsDocString
mkHsDocStringUtf8ByteString = HsDocString

unpackHDS :: HsDocString -> String
unpackHDS = utf8DecodeByteString . hsDocStringToByteString

-- | Return the contents of a 'HsDocString' as a UTF8-encoded 'ByteString'.
hsDocStringToByteString :: HsDocString -> ByteString
hsDocStringToByteString (HsDocString bs) = bs

-- | Join two docstrings.
--
-- Non-empty docstrings are joined with two newlines in between,
-- resulting in separate paragraphs.
appendDocs :: HsDocString -> HsDocString -> HsDocString
appendDocs x y =
  fromMaybe
    (HsDocString BS.empty)
    (concatDocs [x, y])

-- | Concat docstrings with two newlines in between.
--
-- Empty docstrings are skipped.
--
-- If all inputs are empty, 'Nothing' is returned.
concatDocs :: [HsDocString] -> Maybe HsDocString
concatDocs xs = HsDocString b <$ guard (not $ BS.null b)
  where
    b = BS.intercalate (C8.pack "\n\n")
      . filter (not . BS.null)
      . map hsDocStringToByteString
      $ xs

-- | Docs for declarations: functions, data types, instances, methods etc.
newtype DeclDocMap = DeclDocMap (Map Name HsDocString)

instance Binary DeclDocMap where
  put_ bh (DeclDocMap m) = put_ bh (Map.toList m)
  -- We can't rely on a deterministic ordering of the `Name`s here.
  -- See the comments on `Name`'s `Ord` instance for context.
  get bh = DeclDocMap . Map.fromList <$> get bh

instance Outputable DeclDocMap where
  ppr (DeclDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, doc) = ppr name Outputable.<> colon $$ nest 2 (ppr doc)

emptyDeclDocMap :: DeclDocMap
emptyDeclDocMap = DeclDocMap Map.empty

-- | Docs for arguments. E.g. function arguments, method arguments.
newtype ArgDocMap = ArgDocMap (Map Name (Map Int HsDocString))

instance Binary ArgDocMap where
  put_ bh (ArgDocMap m) = put_ bh (Map.toList (Map.toAscList <$> m))
  -- We can't rely on a deterministic ordering of the `Name`s here.
  -- See the comments on `Name`'s `Ord` instance for context.
  get bh = ArgDocMap . fmap Map.fromDistinctAscList . Map.fromList <$> get bh

instance Outputable ArgDocMap where
  ppr (ArgDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, int_map) =
        ppr name Outputable.<> colon $$ nest 2 (pprIntMap int_map)
      pprIntMap im = vcat (map pprIPair (Map.toAscList im))
      pprIPair (i, doc) = ppr i Outputable.<> colon $$ nest 2 (ppr doc)

emptyArgDocMap :: ArgDocMap
emptyArgDocMap = ArgDocMap Map.empty
