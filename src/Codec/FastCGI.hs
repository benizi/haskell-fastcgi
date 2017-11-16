module Codec.FastCGI where

import Control.Monad (foldM)
import Data.Binary (Binary(..), Get, Put, Word8, Word16, Word32, encode)
import Data.Binary.Get
  ( getWord16be
  , getWord32be
  , getWord8
  , getByteString
  , skip
  )
import Data.Binary.Put (putWord16be, putWord32be, putWord8, putByteString)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default(..), def)

data Record = Record
  { rHeader :: Header
  , rContent :: B.ByteString
  } deriving (Show)

putNonEmpty :: B.ByteString -> Put
putNonEmpty s = if B.length s > 0 then putByteString s else return ()

instance Binary Record where
  get = do
    h <- get
    c <- getByteString $ fromIntegral $ hContentLength h
    skip $ fromIntegral $ hPaddingLength h
    return $ Record h c
  put (Record header content) = do
    put fixedHeader
    putByteString content
    putByteString padding
      where
        fixedHeader = header { hContentLength = clen, hPaddingLength = plen8 }
        clen = fromIntegral $ B.length content
        align = clen `mod` 8
        plen = fromIntegral $ if align == 0 then 0 else 8 - align
        plen8 = toEnum plen
        padding = B.pack $ replicate plen 0

getRecord :: Binary b => (Header -> b -> r) -> Get r
getRecord ctor = do
  header <- get
  body <- get
  return $ ctor header body

putRecord :: Binary b => Header -> b -> Put
putRecord header body =
  let content = toStrict $ encode body
   in put $ Record header content

{-
typedef struct {
    unsigned char nameLengthB0;  /* nameLengthB0  >> 7 == 0 */
    unsigned char valueLengthB0; /* valueLengthB0 >> 7 == 0 */
    unsigned char nameData[nameLength];
    unsigned char valueData[valueLength];
} FCGI_NameValuePair11;

typedef struct {
    unsigned char nameLengthB0;  /* nameLengthB0  >> 7 == 0 */
    unsigned char valueLengthB3; /* valueLengthB3 >> 7 == 1 */
    unsigned char valueLengthB2;
    unsigned char valueLengthB1;
    unsigned char valueLengthB0;
    unsigned char nameData[nameLength];
    unsigned char valueData[valueLength
            ((B3 & 0x7f) << 24) + (B2 << 16) + (B1 << 8) + B0];
} FCGI_NameValuePair14;

typedef struct {
    unsigned char nameLengthB3;  /* nameLengthB3  >> 7 == 1 */
    unsigned char nameLengthB2;
    unsigned char nameLengthB1;
    unsigned char nameLengthB0;
    unsigned char valueLengthB0; /* valueLengthB0 >> 7 == 0 */
    unsigned char nameData[nameLength
            ((B3 & 0x7f) << 24) + (B2 << 16) + (B1 << 8) + B0];
    unsigned char valueData[valueLength];
} FCGI_NameValuePair41;

typedef struct {
    unsigned char nameLengthB3;  /* nameLengthB3  >> 7 == 1 */
    unsigned char nameLengthB2;
    unsigned char nameLengthB1;
    unsigned char nameLengthB0;
    unsigned char valueLengthB3; /* valueLengthB3 >> 7 == 1 */
    unsigned char valueLengthB2;
    unsigned char valueLengthB1;
    unsigned char valueLengthB0;
    unsigned char nameData[nameLength
            ((B3 & 0x7f) << 24) + (B2 << 16) + (B1 << 8) + B0];
    unsigned char valueData[valueLength
            ((B3 & 0x7f) << 24) + (B2 << 16) + (B1 << 8) + B0];
} FCGI_NameValuePair44;
-}

data NameValuePair = NameValuePair B.ByteString B.ByteString
  deriving (Show)

get14Length :: Get Int
get14Length = do
  byte0 <- getWord8
  if byte0 `shiftR` 7 == 0
     then return $ fromIntegral byte0
     else let addByte = \tot byte -> byte >>= pure . (tot +) . fromIntegral
              start = fromIntegral $ byte0 .&. 0x7f
           in foldM addByte start $ replicate 3 getWord8

put14Length :: Int -> Put
put14Length n = do
  if n > 0x7f
     then do
       putWord8 $ fromIntegral $ ((n `shiftR` 24) + 0x80) .&. 0xff
       putWord8 $ fromIntegral $ ((n `shiftR` 16) + 0x00) .&. 0xff
       putWord8 $ fromIntegral $ ((n `shiftR` 8) + 0x00) .&. 0xff
       putWord8 $ fromIntegral $ ((n `shiftR` 0) + 0x00) .&. 0xff
     else putWord8 $ fromIntegral $ n

instance Binary NameValuePair where
  get = do
    namelen <- get14Length
    valuelen <- get14Length
    name <- getByteString namelen
    val <- getByteString valuelen
    return $ NameValuePair name val
  put (NameValuePair name val) = do
    put14Length $ B.length name
    put14Length $ B.length val
    putByteString name
    putByteString val

{-
typedef struct {
    unsigned char version;
    unsigned char type;
    unsigned char requestIdB1;
    unsigned char requestIdB0;
    unsigned char contentLengthB1;
    unsigned char contentLengthB0;
    unsigned char paddingLength;
    unsigned char reserved;
} FCGI_Header;
-}

data Header = Header
  { hVersion :: Word8
  , hType :: HeaderType -- Word8
  , hRequestId :: Word16
  , hContentLength :: Word16
  , hPaddingLength :: Word8
  , hReserved :: Word8
  } deriving (Show)

instance Default Header where
  def = Header
    { hVersion = 1
    , hType = UNKNOWN_TYPE
    , hRequestId = 1
    , hContentLength = 0
    , hPaddingLength = 0
    , hReserved = 0
    }

instance Binary Header where
  get = do
    v <- getWord8
    t <- get
    rid <- getWord16be
    len <- getWord16be
    pad <- getWord8
    res <- getWord8
    return $ Header
      { hVersion = v
      , hType = t
      , hRequestId = rid
      , hContentLength = len
      , hPaddingLength = pad
      , hReserved = res
      }
  -- TODO: clean up:
  put (Header v t i len pad res) = do
    putWord8 v
    put t
    putWord16be i
    putWord16be len
    putWord8 pad
    putWord8 res

{-
#define FCGI_BEGIN_REQUEST       1
#define FCGI_ABORT_REQUEST       2
#define FCGI_END_REQUEST         3
#define FCGI_PARAMS              4
#define FCGI_STDIN               5
#define FCGI_STDOUT              6
#define FCGI_STDERR              7
#define FCGI_DATA                8
#define FCGI_GET_VALUES          9
#define FCGI_GET_VALUES_RESULT  10
#define FCGI_UNKNOWN_TYPE       11
#define FCGI_MAXTYPE (FCGI_UNKNOWN_TYPE)
-}

data HeaderType = BEGIN_REQUEST     --   1
                | ABORT_REQUEST     --   2
                | END_REQUEST       --   3
                | PARAMS            --   4
                | STDIN             --   5
                | STDOUT            --   6
                | STDERR            --   7
                | DATA              --   8
                | GET_VALUES        --   9
                | GET_VALUES_RESULT --  10
                | UNKNOWN_TYPE      --  11
                deriving (Show)

instance Binary HeaderType where
  get = do
    val <- getWord8
    case val of
      1 -> return BEGIN_REQUEST
      2 -> return ABORT_REQUEST
      3 -> return END_REQUEST
      4 -> return PARAMS
      5 -> return STDIN
      6 -> return STDOUT
      7 -> return STDERR
      8 -> return DATA
      9 -> return GET_VALUES
      10 -> return GET_VALUES_RESULT
      11 -> return UNKNOWN_TYPE
      _ -> fail $ "Invalid header type value: " ++ show val
  put = putWord8 . convert
    where
      convert BEGIN_REQUEST = 1
      convert ABORT_REQUEST = 2
      convert END_REQUEST = 3
      convert PARAMS = 4
      convert STDIN = 5
      convert STDOUT = 6
      convert STDERR = 7
      convert DATA = 8
      convert GET_VALUES = 9
      convert GET_VALUES_RESULT = 10
      convert UNKNOWN_TYPE = 11

{-
typedef struct {
    unsigned char roleB1;
    unsigned char roleB0;
    unsigned char flags;
    unsigned char reserved[5];
} FCGI_BeginRequestBody;
-}

data BeginRequestBody = BeginRequestBody
  { brbRole :: BRBRole -- Word16
  , brbFlags :: Word8
  , brbReserved :: [Word8] -- [5]
  } deriving (Show)

instance Binary BeginRequestBody where
  get = do
    role <- get
    flags <- getWord8
    res <- B.unpack <$> getByteString 5
    return $ BeginRequestBody role flags res
  put body = do
    put role
    putWord8 flags
    mapM_ putWord8 res
      where
        role = brbRole body
        flags = brbFlags body
        res = brbReserved body

{-
typedef struct {
    FCGI_Header header;
    FCGI_BeginRequestBody body;
} FCGI_BeginRequestRecord;
-}

data BeginRequestRecord = BeginRequestRecord Header BeginRequestBody
  deriving (Show)

instance Binary BeginRequestRecord where
  get = getRecord BeginRequestRecord
  put (BeginRequestRecord header body) = putRecord header body

{-
/*
 * Mask for flags component of FCGI_BeginRequestBody
 */
#define FCGI_KEEP_CONN  1

/*
 * Values for role component of FCGI_BeginRequestBody
 */
#define FCGI_RESPONDER  1
#define FCGI_AUTHORIZER 2
#define FCGI_FILTER     3
-}

data BRBRole = RoleResponder
             | RoleAuthorizer
             | RoleFilter
             deriving (Show)

instance Binary BRBRole where
  get = do
    val <- getWord16be
    case val of
      1 -> return RoleResponder
      2 -> return RoleAuthorizer
      3 -> return RoleFilter
      _ -> fail $ "Unknown role value: " ++ show val
  put = putWord8 . convert
    where
      convert RoleResponder = 1
      convert RoleAuthorizer = 2
      convert RoleFilter = 3

{-
typedef struct {
    unsigned char appStatusB3;
    unsigned char appStatusB2;
    unsigned char appStatusB1;
    unsigned char appStatusB0;
    unsigned char protocolStatus;
    unsigned char reserved[3];
} FCGI_EndRequestBody;
-}

data EndRequestBody = EndRequestBody
  { erbAppStatus :: Word32
  , erbProtocolStatus :: ErbProtocolStatus -- Word8
  , erbReserved :: [Word8] -- [3]
  } deriving (Show)

instance Default EndRequestBody where
  def = EndRequestBody
    { erbAppStatus = 0
    , erbProtocolStatus = ProtoRequestComplete
    , erbReserved = [0,0,0]
    }

instance Binary EndRequestBody where
  get = do
    status <- getWord32be
    proto <- get
    res <- B.unpack <$> getByteString 3
    return $ EndRequestBody
      { erbAppStatus = status
      , erbProtocolStatus = proto
      , erbReserved = res
      }
  put body = do
    putWord32be status
    put proto
    mapM_ putWord8 res
      where
        status = erbAppStatus body
        proto = erbProtocolStatus body
        res = erbReserved body

{-
typedef struct {
    FCGI_Header header;
    FCGI_EndRequestBody body;
} FCGI_EndRequestRecord;
-}

data EndRequestRecord = EndRequestRecord Header EndRequestBody
  deriving (Show)

instance Default EndRequestRecord where
  def = EndRequestRecord header def
    where header = def { hType = END_REQUEST }

instance Binary EndRequestRecord where
  get = getRecord EndRequestRecord
  put (EndRequestRecord header body) = putRecord header body

{-
/*
 * Values for protocolStatus component of FCGI_EndRequestBody
 */
#define FCGI_REQUEST_COMPLETE 0
#define FCGI_CANT_MPX_CONN    1
#define FCGI_OVERLOADED       2
#define FCGI_UNKNOWN_ROLE     3
-}

data ErbProtocolStatus = ProtoRequestComplete
                       | ProtoCantMpxConn
                       | ProtoOverloaded
                       | ProtoUnknownRole
                       deriving (Show)

instance Binary ErbProtocolStatus where
  get = do
    val <- getWord8
    case val of
      0 -> return ProtoRequestComplete
      1 -> return ProtoCantMpxConn
      2 -> return ProtoOverloaded
      3 -> return ProtoUnknownRole
      _ -> fail $ "Unknown role value: " ++ show val
  put = putWord8 . convert
    where
      convert ProtoRequestComplete = 0
      convert ProtoCantMpxConn = 1
      convert ProtoOverloaded = 2
      convert ProtoUnknownRole = 3

{-
/*
 * Variable names for FCGI_GET_VALUES / FCGI_GET_VALUES_RESULT records
 */
#define FCGI_MAX_CONNS  "FCGI_MAX_CONNS"
#define FCGI_MAX_REQS   "FCGI_MAX_REQS"
#define FCGI_MPXS_CONNS "FCGI_MPXS_CONNS"
-}

{-
typedef struct {
    unsigned char type;
    unsigned char reserved[7];
} FCGI_UnknownTypeBody;
-}

data UnknownTypeBody = UnknownTypeBody
  { utbType :: Word8
  , utbReserved :: [Word8] -- [7]
  } deriving (Show)

instance Binary UnknownTypeBody where
  get = do
    t <- getWord8
    r <- B.unpack <$> getByteString 7
    return $ UnknownTypeBody
      { utbType = t
      , utbReserved = r
      }
  put body = do
    putWord8 btype
    mapM_ putWord8 res
      where
        btype = utbType body
        res = utbReserved body

{-
typedef struct {
    FCGI_Header header;
    FCGI_UnknownTypeBody body;
} FCGI_UnknownTypeRecord;
-}

data UnknownTypeRecord = UnknownTypeRecord Header UnknownTypeBody
  deriving (Show)

instance Binary UnknownTypeRecord where
  get = getRecord UnknownTypeRecord
  put (UnknownTypeRecord h b) = putRecord h b
