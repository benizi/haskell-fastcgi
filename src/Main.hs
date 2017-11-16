{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Codec.FastCGI
  ( BeginRequestBody(..)
  , EndRequestRecord(..)
  , Header(..)
  , HeaderType(..)
  , NameValuePair(..)
  , Record(..)
  )
import Conduit
  ( Conduit
  , MonadIO
  , (.|)
  , await
  , evalStateC
  , leftover
  , liftIO
  , runConduitRes
  , sinkHandle
  , sourceHandle
  , yield
  )
import Control.Monad (when)
import Control.Monad.State (MonadState, modify)
import qualified Control.Monad.State as St
import Data.Binary (Binary, Get, Put, get, put)
import Data.Binary.Get (Decoder(..), pushChunk, runGetOrFail, runGetIncremental)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO
  ( Handle
  , hFlush
  , hGetContents
  , hPutStrLn
  , hSetBinaryMode
  , stderr
  , stdin
  , stdout
  )
import System.Process (CreateProcess)
import qualified System.Process as Proc

warn :: MonadIO m => String -> m ()
warn msg = liftIO (hPutStrLn stderr msg >> hFlush stderr)

debug :: MonadIO m => String -> m ()
#ifdef DEBUG
debug = warn
#else
debug = warn -- TODO build flags? -- debug _ = liftIO $ return ()
#endif

data DecodeState = DecodeState
  { sRecord :: Decoder Record
  , sParams :: ParamState
  , sFinished :: Bool
  }

initial :: DecodeState
initial = DecodeState
  { sRecord = decodeRecord
  , sParams = NoParams
  , sFinished = False
  }

genDecoder :: Binary a => Decoder a
genDecoder = runGetIncremental get

decodeRecord :: Decoder Record
decodeRecord = genDecoder :: Decoder Record

decodeParam :: Decoder NameValuePair
decodeParam = genDecoder :: Decoder NameValuePair

parsestate :: Decoder a -> String
parsestate = \case
  Fail _ _ _ -> "Fail"
  Partial _ -> "Partial"
  Done _ _ _ -> "Done"

readRecords :: (MonadState DecodeState m, MonadIO m)
            => Conduit B.ByteString m Record
readRecords = do
  decoder <- sRecord <$> St.get
  debug $ "readRecords(" ++ parsestate decoder ++ ")"
  await >>= \case
    Nothing -> debug "DONE in readRecords" >> return ()
    Just chunk -> input decoder chunk >> readRecords

input :: (MonadState DecodeState m, MonadIO m)
      => Decoder Record
      -> B.ByteString
      -> Conduit B.ByteString m Record
input decoder chunk = do
  case decoder' of
    Done remaining _ record -> do
      leftover remaining
      yield record
      setDecoder decodeRecord
    _ -> setDecoder decoder'
  where
    decoder' = pushChunk decoder chunk
    setDecoder d = modify $ \s -> s { sRecord = d }

data ParamState = NoParams
                | SomeParams (Decoder NameValuePair) [NameValuePair]
                | GotParams [NameValuePair]

noop :: MonadIO m => m ()
noop = liftIO $ return ()

dumper :: MonadIO m => B.ByteString -> m ()
dumper response = warnout (B.length response) >> start response
  where
    warnout a = debug $ " => " ++ show a
    start = step decodeRecord
    step decoder chunk =
      let decoder' = pushChunk decoder chunk
       in case decoder' of
            Done remaining _ (Record h b) -> do
              warnout h
              debug $ "    " ++ show b
              if B.length remaining > 0
                 then start remaining
                 else noop
            Fail _ _ _ -> debug "FAIL"
            Partial _ -> debug "INCOMPLETE"

parseRequest :: (MonadState DecodeState m, MonadIO m)
             => Conduit Record m DecodeState
parseRequest = do
  debug "AWAIT in parseRequest"
  mrec <- await
  case mrec of
    Nothing -> debug "DONE in parseRequest"
    Just (Record header content) -> do
      debug $ " <= " ++ show header
      let lazy = BL.fromStrict content
      case hType header of
        BEGIN_REQUEST ->
          let parser = runGetOrFail (get :: Get BeginRequestBody)
           in case parser lazy of
                Left (_,_,msg) -> debug msg
                Right (_,_,body) -> debug $ show body
        PARAMS -> do
          paramstate <- sParams <$> St.get
          case paramstate of
            GotParams params ->
              if B.length content == 0
                 then debug "Parsed all params"
                 else readParams (SomeParams decodeParam params) content
            _ -> readParams paramstate content
        STDIN -> do
          if B.length content == 0
             then finish
             else noop
        _ -> let msg = "Unhandled header type: " ++ (show $ hType header)
              in debug msg
  done <- sFinished <$> St.get
  if done
     then debug "DONE at end of parseRequest"
     else debug "LOOPING" >> parseRequest
    where
      finish = do
        -- modify $ \s -> s { sFinished = True }
        st <- St.get
        yield st

headerFor :: HeaderType -> Header
headerFor t = def { hType = t }

putStream :: HeaderType -> B.ByteString -> Put
putStream stream contents = do
  when (contents /= B.empty) $ write contents
  write B.empty
    where
      write = put . Record header
      header = headerFor stream

putResponseBoth :: B.ByteString -> B.ByteString -> Put
putResponseBoth out err = do
  writeIf STDOUT out
  writeIf STDERR err
  put (def :: EndRequestRecord)
    where
      writeIf stream contents =
        when (contents /= B.empty) (putStream stream contents)

putResponse :: B.ByteString -> Put
putResponse = flip putResponseBoth B.empty

responseFor :: B.ByteString -> B.ByteString
responseFor = BL.toStrict . runPut . putResponse

complexResponse :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
complexResponse headers body errors =
  BL.toStrict $ runPut $ putResponseBoth out errors
    where
      out = B.append headers body

headerData :: [(String,String)] -> B.ByteString
headerData = join . fmap header
  where
    header (k,v) = C.pack $ k ++ ": " ++ v
    join headers = B.intercalate crlf (headers ++ [crlf])
    crlf = C.pack "\r\n"

readParams :: (MonadState DecodeState m, MonadIO m)
           => ParamState
           -> B.ByteString
           -> Conduit Record m DecodeState
readParams paramstate content =
  let (decoder, acc) = fromState paramstate
      decoder' = pushChunk decoder content
   in do
     case decoder' of
       Done remaining _ pair -> do
         let tot = (pair:acc)
          in if remaining == B.empty
                then do
                  setState $ GotParams tot
                else readParams (SomeParams decodeParam tot) remaining
       _ -> do
         setState $ SomeParams decoder' acc
         noop
    where
      setState p = modify $ \s -> s { sParams = p }
      fromState (SomeParams decoder acc) = (decoder, acc)
      fromState (GotParams acc) = (decodeParam, acc)
      fromState NoParams = (decodeParam, [])

type EnvString = (String,String)

data ScriptResult = ScriptResult
  { srOut :: String
  , srErr :: String
  , srExit :: ExitCode
  , srEnv :: [EnvString]
  , srSsiVars :: [EnvString]
  }

runProgram :: MonadIO m
           => Conduit DecodeState m ScriptResult
runProgram = do
  mst <- await
  case mst of
    Nothing -> noop
    Just st -> result (sParams st)
  where
    result :: MonadIO m => ParamState -> Conduit DecodeState m ScriptResult
    result = runscript . pairs

    pairs :: ParamState -> [EnvString]
    pairs (GotParams ps) = kvstring <$> ps
    pairs _ = []
    kvstring :: NameValuePair -> EnvString
    kvstring (NameValuePair k v) = (C.unpack k, C.unpack v)

    progname :: [EnvString] -> String
    progname params =
      let progvar = (`elem` ["SSI_EXE","SSI_PROG"])
       in maybe "" id $ listToMaybe $ snd <$> filter (progvar . fst) params

    creator :: [EnvString] -> CreateProcess
    creator params =
      (Proc.proc (progname params) [])
        { Proc.std_out = Proc.CreatePipe
        , Proc.std_err = Proc.CreatePipe
        , Proc.env = Just params
        }

    ssi :: MonadIO m => [EnvString] -> m (Handle, Handle, Proc.ProcessHandle)
    ssi env = liftIO $ do
      (_, Just oh, Just eh, ph) <- Proc.createProcess $ creator env
      return $ (oh, eh, ph)

    runscript :: MonadIO m => [EnvString] -> Conduit DecodeState m ScriptResult
    runscript ssivars = do
      env <- fullEnv ssivars
      ret <- liftIO $ runSsi env ssivars
      yield ret
      return () -- TODO this will cancel?

    runSsi :: MonadIO m => [EnvString] -> [EnvString] -> m ScriptResult
    runSsi env ssivars = liftIO $ do
      (outH, errH, proc) <- ssi env
      out <- hGetContents outH
      err <- hGetContents errH
      exit <- Proc.waitForProcess proc
      return $
        ScriptResult out err exit (List.sortOn fst env) (List.sortOn fst ssivars)

    fullEnv fromssi = do
      fromenv <- liftIO getEnvironment
      return $ uniqByKey $ fromenv ++ fromssi
        where
          comparing f a b = f a == f b
          uniqByKey = List.nubBy (comparing fst)

generateResponse :: MonadIO m
                 => Conduit ScriptResult m B.ByteString
generateResponse = do
  debug "AWAIT in generateResponse"
  inp <- await
  case inp of
    Nothing -> noop
    Just scriptresult -> do
      debug $ interesting scriptresult
      case scriptresult of
        ScriptResult { srExit = ExitSuccess } -> noop
        _ -> debug $ interesting scriptresult
      yield (response scriptresult)
  warn $ "END of generateResponse"
  return ()
  where
    response = \case
      r@(ScriptResult out _ ExitSuccess _ _) ->
        case lines out of
          (_:_:_) -> good out
          _ -> bad r
      result -> bad result
    good = withType "text/html"
    bad r = complexResponse (ctheader "text/plain") (failed r) (stderr' r)
    stderr' = C.pack . dbginfo . pairs
    failed (ScriptResult _ _ ExitSuccess _ _) = C.pack "Something went wrong"
    failed (ScriptResult _ _ code _ _) = C.pack $ show code
    pairs (ScriptResult o e c env senv) = [ ("EXIT",show c)
                                          , ("OUT",o)
                                          , ("ERR",e)
                                          , ("ENV",dump env)
                                          , ("SSI",dump senv)
                                          ]
    dbginfo = unlines . fmap section
    section (k,v) = k ++ ":\n  " ++ v
    dump = ("\n" ++) . unlines . fmap kv
    kv (k,v) = "  " ++ k ++ "=[" ++ v ++ "]"
    withType ct = responseFor . B.append (ctheader ct) . C.pack
    ctheader ct = headerData [("Content-Type",ct)]
    interesting = dbginfo . filter bysection . pairs
    bysection = (`elem` ["ERR","SSI"]) . fst

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  runConduitRes $ evalStateC initial $
    sourceHandle stdin
      .| readRecords
      .| parseRequest
      .| runProgram
      .| generateResponse
      .| sinkHandle stdout
