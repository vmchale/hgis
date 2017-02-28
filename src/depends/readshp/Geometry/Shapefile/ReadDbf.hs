{-|
Module      : Geometry.Shapefile.ReadDbf
Description : Code for reading DBF files.
              Not robust, handle with care. YMMV
              Based on http://www.dbf2002.com/dbf-file-format.html
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module Geometry.Shapefile.ReadDbf ( readDbfFile,
                                    readDbfData
                                  ) where

import Control.Monad       ( replicateM )
import Control.Monad.Loops ( whileM )
import Data.Binary.Get hiding (getInt8)

import qualified Data.ByteString.Lazy as BL

import Geometry.Shapefile.Internal
import Geometry.Shapefile.Types

-- | Read dbf file at `fp` into resident data format `DbfData`
readDbfFile :: String -> IO DbfData
readDbfFile fp = readDbfData <$> BL.readFile fp

-- | Parse a ByteString containing DBF data
readDbfData :: BL.ByteString -> DbfData
readDbfData = runGet $ do
  _           <- getByteString 4   -- DBF File type
  numRecs     <- getIntLE          -- Number of records
  firstRecPos <- getInt16LE        -- Position of first record
  _           <- getByteString 22  -- Rec len, table flags, code page mark
  -- Field descriptors
  let fieldsRemain = (< firstRecPos - 1) . fromIntegral <$> bytesRead
  fields      <- whileM fieldsRemain getDbfFieldDesc
  _           <- getWord8          -- Skip spacer
  -- Records
  recs        <- replicateM numRecs (getWord8 >> mapM getDbfRecord fields)
  -- Everything together
  return DbfData { dbfNumRecs = numRecs,
              dbfFields  = fields,
              dbfRecords = recs }

-- | Read header field descriptor
getDbfFieldDesc :: Get DbfFieldDesc
getDbfFieldDesc = do
  name     <- getString 11     -- Field name
  fieldT   <- getCharVal       -- Field type
  _        <- getIntLE         -- Field displacement (?)
  len      <- getInt8          -- Field length
  _        <- getByteString 15 -- Num decimal/field flags/autoincrement
  return DbfFieldDesc { fieldName = name,
                   fieldType = fieldT,
                   fieldLen  = len }

-- | Read a record. Not all record types are (meaningfully) implemented.
getDbfRecord :: DbfFieldDesc -> Get DbfRecord
getDbfRecord field = let n = fieldLen field in
  case fieldType field of
    'C' -> DbfString <$> getString     n
    'N' -> DbfNum    <$> getString     n
    _   -> DbfBS     <$> getByteString n

