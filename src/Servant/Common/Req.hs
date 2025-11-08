{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Common.Req (module Servant.Common.Req, ReqResultRaw(.., ResponseSuccess)) where

-------------------------------------------------------------------------------
#if MIN_VERSION_base(4,18,0)
import           Control.Applicative        (liftA3)
#else
import           Control.Applicative     (liftA2, liftA3)
#endif
import           Control.Arrow              ((&&&))
import           Control.Concurrent
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Builder    as Builder
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString            (ByteString)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Functor.Compose
import           Data.Proxy                 (Proxy(..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Traversable           (forM, for)
import           Language.Javascript.JSaddle.Monad (JSM, MonadJSM, liftJSM)
import qualified Network.URI                as N
import           Reflex.Dom.Core                 hiding (tag)
import           Servant.Common.BaseUrl     (BaseUrl, showBaseUrl,
                                             SupportsServantReflex)
import           Servant.API.ContentTypes   (MimeUnrender(..), NoContent(..))
import           Web.HttpApiData            (ToHttpApiData(..))
import qualified Data.CaseInsensitive    as CI
-------------------------------------------------------------------------------
import           Servant.API.BasicAuth
import Data.Kind (Type)
import Data.Default (Default)
import Data.Foldable (Foldable)

------------------------------------------------------------------------------
-- | The result of a request event
data ReqResultRaw tag t m a r where
    ResponseSuccess' :: forall tag a r t m. Requester t m => tag -> r -> Response m a -> ReqResultRaw tag t m a r
      -- ^ The successfully decoded response from a request tagged with 'tag'
    ResponseFailure :: forall tag t m a r. Requester t m => tag -> Text -> Response m a -> ReqResultRaw tag t m a r
      -- ^ The failure response, which may have failed decoding or had
      --   a non-successful response code
    RequestFailure :: tag -> Text -> ReqResultRaw tag t m a r
      -- ^ A failure to construct the request tagged with 'tag' at trigger time

type ReqResult tag t m r = ReqResultRaw tag t m XhrPayload r

pattern ResponseSuccess :: forall tag a t m. Requester t m => tag -> a -> Response m a -> ReqResultRaw tag t m a a
pattern ResponseSuccess tag r resp = ResponseSuccess' tag r resp

instance (Requester t m) => Functor (ReqResultRaw tag t m p) where
  fmap :: forall a b. (Requester t m) => (a -> b) -> ReqResultRaw tag t m p a -> ReqResultRaw tag t m p b
  fmap _ (RequestFailure tag tx) = RequestFailure tag tx
  fmap _ (ResponseFailure tag tx resp) = ResponseFailure tag tx resp
  fmap f (ResponseSuccess' tag a resp) = ResponseSuccess' tag (f a) resp

instance (Requester t m) => Foldable (ReqResultRaw tag t m a) where
  foldMap f (ResponseSuccess' _ a _) = f a
  foldMap _ _ = mempty

instance (Requester t m) => Traversable (ReqResultRaw tag t m a) where
  sequenceA (RequestFailure tag tx) = pure $ RequestFailure tag tx
  sequenceA (ResponseFailure tag tx resp) = pure $ ResponseFailure tag tx resp
  sequenceA (ResponseSuccess' tag a resp) = ffor a $ \t -> ResponseSuccess' tag t resp

newtype ClientOptions m = ClientOptions
    { optsRequestFixup :: forall a {t}. Requester t m => Request m a -> JSM (Request m a)
      -- ^ Aribtrarily modify requests just before they are sent.
      -- Warning: This escape hatch opens the possibility for your
      -- requests to diverge from what the server expects, when the
      -- server is also derived from a servant API
    }

defaultClientOptions :: ClientOptions m
defaultClientOptions = ClientOptions pure

-- withCredentials :: Lens' (XhrRequest a) Bool
withCredentials :: (Functor f) => (Bool -> f Bool) -> XhrRequest a -> f (XhrRequest a)
withCredentials inj r@(XhrRequest _ _ cfg) =
    let cfg' = (\b -> cfg { _xhrRequestConfig_withCredentials = b}) <$>
               inj (_xhrRequestConfig_withCredentials cfg)
    in (\c' -> r {_xhrRequest_config = c' }) <$> cfg'

------------------------------------------------------------------------------
-- | Simple filter/accessor for successful responses, when you want to
-- ignore the error case. For example:
-- >> goodResponses <- fmapMaybe reqSuccess <$> clientFun triggers
reqSuccess :: ReqResultRaw tag t m a r -> Maybe r
reqSuccess (ResponseSuccess' _ x _) = Just x
reqSuccess _                       = Nothing


------------------------------------------------------------------------------
-- | Simple filter/accessor like 'reqSuccess', but keeping the request tag
reqSuccess' :: ReqResultRaw tag t m a r -> Maybe (tag,r)
reqSuccess' (ResponseSuccess' tag x _) = Just (tag,x)
reqSuccess' _                         = Nothing


------------------------------------------------------------------------------
-- | Simple filter/accessor for any failure case
reqFailure :: ReqResultRaw tag t m a r -> Maybe Text
reqFailure (ResponseFailure _ s _) = Just s
reqFailure (RequestFailure  _ s)   = Just s
reqFailure _                       = Nothing

------------------------------------------------------------------------------
-- | Simple filter/accessor like 'reqFailure', but keeping the request tag
reqFailure' :: ReqResultRaw tag t m a r -> Maybe (tag,Text)
reqFailure' (ResponseFailure tag s _) = Just (tag,s)
reqFailure' (RequestFailure  tag s)   = Just (tag,s)
reqFailure' _                         = Nothing


------------------------------------------------------------------------------
-- | Simple filter/accessor for the raw XHR response
response :: forall tag a r t m. Requester t m => ReqResultRaw tag t m a r -> Maybe (Response m a)
response (ResponseSuccess' _ _ x) = Just x
response (ResponseFailure _ _ x) = Just x
response _                       = Nothing

------------------------------------------------------------------------------
-- | Retrieve response tag
reqTag :: ReqResultRaw tag t m a r -> tag
reqTag (ResponseSuccess' tag _ _) = tag
reqTag (ResponseFailure tag _ _) = tag
reqTag (RequestFailure  tag _  ) = tag

-------------------------------------------------------------------------------
-- | You must wrap the parameter of a QueryParam endpoint with 'QParam' to
-- indicate whether the parameter is valid and present, validly absent, or
-- invalid
data QParam a = QParamSome a
              -- ^ A valid query parameter
              | QNone
              -- ^ Indication that the parameter is intentionally absent (the request is valid)
              | QParamInvalid Text
              -- ^ Indication that your validation failed (the request isn't valid)


qParamToQueryPart :: ToHttpApiData a => QParam a -> Either Text (Maybe Text)
qParamToQueryPart (QParamSome a)    = Right (Just $ toQueryParam a)
qParamToQueryPart QNone             = Right Nothing
qParamToQueryPart (QParamInvalid e) = Left e


data QueryPart t = QueryPartParam  (Dynamic t (Either Text (Maybe Text)))
                 | QueryPartParams (Dynamic t [Text])
                 | QueryPartFlag   (Dynamic t Bool)


-------------------------------------------------------------------------------
-- The data structure used to build up request information while traversing
-- the shape of a servant API
data Req t = Req
  { reqMethod    :: Text
  , reqPathParts :: [Dynamic t (Either Text Text)]
  , qParams      :: [(Text, QueryPart t)]
  , reqBody      :: Maybe (Dynamic t (Either Text (BL.ByteString, Text)))
  , headers      :: [(Text, Dynamic t (Either Text Text))]
  , respHeaders  :: XhrResponseHeaders
  , authData     :: Maybe (Dynamic t (Maybe BasicAuthData))
  }

defReq :: Req t
defReq = Req "GET" [] [] Nothing [] def Nothing

prependToPathParts :: Dynamic t (Either Text Text) -> Req t -> Req t
prependToPathParts p req =
  req { reqPathParts = p : reqPathParts req }

addHeader :: (ToHttpApiData a, Reflex t) => Text -> Dynamic t (Either Text a) -> Req t -> Req t
addHeader name val req = req { headers = (name, (fmap . fmap) (TE.decodeUtf8 . toHeader) val) : headers req }

instance Default XhrPayload where
  def = mempty

class (Requester t m) => RequestConfig t m where
  type RConfig t m :: Type -> Type
  defa :: Default a => RConfig t m a
  getUrl :: Request m a -> Text
  getUser :: RConfig t m a -> Maybe Text
  getHeaders :: RConfig t m a -> Map.Map Text Text
  getPassword :: RConfig t m a -> Maybe Text
  getWithCredentials :: RConfig t m a -> Bool
  getSendData :: RConfig t m a -> a
  getResponseType :: RConfig t m a -> Maybe XhrResponseType
  getRequestHeaders :: RConfig t m a -> XhrResponseHeaders
  setUrl :: Request m a -> Text -> Request m a
  setUser :: RConfig t m a -> Maybe Text -> RConfig t m a
  setHeaders :: RConfig t m a -> Map.Map Text Text -> RConfig t m a
  setPassword :: RConfig t m a -> Maybe Text -> RConfig t m a
  setWithCredentials :: RConfig t m a -> Bool -> RConfig t m a
  setSendData :: RConfig t m a -> a -> RConfig t m a
  setResponseType :: RConfig t m a -> Maybe XhrResponseType -> RConfig t m a
  setRequestHeaders :: RConfig t m a -> XhrResponseHeaders -> RConfig t m a
  build :: Text -> Text -> RConfig t m a -> Request m a

class (Requester t m) => ResponseData t m where
  getStatus :: Response m a -> Int
  getResponseText :: Response m a -> Maybe Text
  getResponseBody :: Response m a -> Maybe XhrResponseBody
  getResponseHeaders :: Response m a -> Map.Map (CI.CI Text) Text


reqToReflexRequest
    :: forall t m. RequestConfig t m
    => Text
    -> Dynamic t BaseUrl
    -> Req t
    -> Dynamic t (Either Text (Request m XhrPayload))
reqToReflexRequest reqMeth reqHost req =
  let t :: Dynamic t [Either Text Text]
      t = sequence $ reverse $ reqPathParts req

      baseUrl :: Dynamic t (Either Text Text)
      baseUrl = Right . showBaseUrl <$> reqHost

      urlParts :: Dynamic t (Either Text [Text])
      urlParts = fmap sequence t

      urlPath :: Dynamic t (Either Text Text)
      urlPath = (fmap.fmap)
                (T.intercalate "/" . fmap escape)
                urlParts

      queryPartString :: (Text, QueryPart t) -> Dynamic t (Maybe (Either Text Text))
      queryPartString (pName, qp) = case qp of
        QueryPartParam p -> ffor p $ \case
          Left e         -> Just (Left e)
          Right (Just a) -> Just (Right $ pName <> "=" <> escape a)
          Right Nothing  -> Nothing
        QueryPartParams ps -> ffor ps $ \pStrings ->
          if null pStrings
          then Nothing
          else Just . Right
               . T.intercalate "&"
               $ fmap (\p -> pName <> "=" <> escape p) pStrings
        QueryPartFlag fl -> ffor fl $ \case
          True ->  Just $ Right pName
          False -> Nothing


      queryPartStrings :: [Dynamic t (Maybe (Either Text Text))]
      queryPartStrings = map queryPartString (qParams req)
      queryPartStrings' = fmap (sequence . catMaybes) $ sequence queryPartStrings :: Dynamic t (Either Text [Text])
      queryString :: Dynamic t (Either Text Text) =
        ffor queryPartStrings' $ \qs -> fmap (T.intercalate "&") qs
      xhrUrl =  (liftA3 . liftA3) (\a p q -> a </> if T.null q then p else p <> "?" <> q)
          baseUrl urlPath queryString
        where
          (</>) :: Text -> Text -> Text
          x </> y | ("/" `T.isSuffixOf` x) || ("/" `T.isPrefixOf` y) = x <> y
                  | otherwise = x <> "/" <> y


      xhrHeaders :: Dynamic t (Either Text [(Text, Text)])
      xhrHeaders = (fmap sequence . sequence . fmap f . headers) req
        where
          f = \(headerName, dynam) ->
                fmap (fmap (\rightVal -> (headerName, rightVal))) dynam

      mkConfigBody :: RequestConfig t m => Either Text [(Text,Text)]
                   -> (Either Text (BL.ByteString, Text))
                   -> Either Text (RConfig t m XhrPayload)
      mkConfigBody ehs rb = case (ehs, rb) of
                  (_, Left e)                     -> Left e
                  (Left e, _)                     -> Left e
                  (Right hs, Right (bBytes, bCT)) ->
                    Right $ setRequestHeaders @t @m (setResponseType @t @m (setHeaders @t @m (setSendData @t @m (defa @t @m) (bytesToPayload bBytes)) (Map.insert "Content-Type" bCT (Map.fromList hs))) (Just XhrResponseType_ArrayBuffer)) (respHeaders req)

      xhrOpts :: (RequestConfig t m) => Dynamic t (Either Text (RConfig t m XhrPayload))
      xhrOpts = case reqBody req of
        Nothing    -> ffor xhrHeaders $ \case
                               Left e -> Left e
                               Right hs -> Right $ setRequestHeaders @t @m (setResponseType @t @m (setHeaders @t @m (defa @t @m) (Map.fromList hs)) (Just XhrResponseType_ArrayBuffer)) (respHeaders req)
        Just rBody -> liftA2 mkConfigBody xhrHeaders rBody

      mkAuth :: Maybe BasicAuthData -> Either Text (RConfig t m x) -> Either Text (RConfig t m x)
      mkAuth _ (Left e) = Left e
      mkAuth Nothing r  = r
      mkAuth (Just (BasicAuthData u p)) (Right config) = Right $ setPassword @t @m (setUser @t @m config (Just $ TE.decodeUtf8 u)) (Just $ TE.decodeUtf8 p)

      addAuth :: Dynamic t (Either Text (RConfig t m x))
              -> Dynamic t (Either Text (RConfig t m x))
      addAuth xhr = case authData req of
        Nothing -> xhr
        Just auth -> liftA2 mkAuth auth xhr

      xhrReq = (liftA2 . liftA2) (\p opt -> build @t @m reqMeth p opt) xhrUrl (addAuth xhrOpts)

  in xhrReq

-- * performing requests

displayHttpRequest :: Text -> Text
displayHttpRequest httpmethod = "HTTP " <> httpmethod <> " request"

-- | This function performs the request
performRequests :: forall b t m f tag.(SupportsServantReflex t m, Traversable f, Requester t m, RequestConfig t m, MonadHold t m)
                => Text
                -> Dynamic t (f (Req t))
                -> Dynamic t BaseUrl
                -> ClientOptions m
                -> Event t tag
                -> m (Event t (tag, f (Either Text (Response m XhrPayload))))
performRequests reqMeth rs reqHost opts trigger = do
  let
      xhrReqs ::Dynamic t (f (Either Text (Request m XhrPayload)))
      xhrReqs =
          join $ (\(fxhr :: f (Req t)) -> sequence $
                    reqToReflexRequest @t @m reqMeth reqHost <$> fxhr) <$> rs

      -- xhrReqs = fmap snd <$> xhrReqsAndDebugs
      reqs :: Event
                  t (Compose ((,) tag) f (Either Text (Request m XhrPayload)))
      reqs    = attachPromptlyDynWith
                (\fxhr t -> Compose (t, fxhr)) xhrReqs trigger

  (resps :: Event
                  t (Compose ((,) tag) f (Either Text (Response m XhrPayload)))) <- performSomeRequestsAsync opts reqs
  return $ getCompose <$> resps

-- | Issues a collection of requests when the supplied Event fires.
-- When ALL requests from a given firing complete, the results are
-- collected and returned via the return Event.
performSomeRequestsAsync
    :: (MonadIO (Performable m),
        MonadJSM (Performable m),
        PerformEvent t m,
        TriggerEvent t m,
        Requester t m,
        MonadHold t m,
        Traversable f
       )
    => ClientOptions m
    -> Event t (f (Either Text (Request m a)))
    -> m (Event t (f (Either Text (Response m a))))
performSomeRequestsAsync opts =
    performSomeRequestsAsync' opts () . fmap return


------------------------------------------------------------------------------
-- | A modified version or Reflex.Dom.Xhr.performRequestsAsync
-- that accepts 'f (Either e (XhrRequestb))' events
performSomeRequestsAsync'
    :: (MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m, Requester t m, Traversable f, MonadHold t m)
    => ClientOptions m
    -> () -- Removed function
    -> Event t (Performable m (f (Either Text (Request m a)))) -> m (Event t (f (Either Text (Response m a))))
performSomeRequestsAsync' opts newXhr req = do
  reqs <- performEventAsync $ ffor req $ \hrs cb -> do
    rs <- hrs
    reqs' <- forM rs $ \case
        Right r -> do
          r' <- liftJSM $ optsRequestFixup opts r
          --r'' <- newXhr r' (const $ pure ())
          pure $ Right r'
        (Left e) -> pure (Left e)
    liftIO $ forkIO $ () <$ for reqs' (\t -> cb (t , reqs'))
    pure ()

  let (left, right) = fanEither $ fmap fst reqs
  d <- holdDyn Nothing $ fmap (Just . snd) reqs
  resps <- requesting $ right
  let eresps = leftmost [ffor left Left, ffor (resps) Right]
  pure $ attachPromptlyDynWithMaybe (\m e -> ffor m $ fmap (const e)) d eresps
  {-
  rs <- hrs
  resps <- forM rs $ \r -> case r of
      Left e -> do
          resp <- liftIO $ newMVar (Left e)
          return resp
      Right r' -> do
          resp <- liftIO newEmptyMVar
          r'' <- liftJSM $ (optsRequestFixup opts) r'
          _ <- newXhr r'' $ liftIO . putMVar resp . Right
          return resp
  _ <- liftIO $ forkIO $ cb =<< forM resps takeMVar
  return ()
  -}



type XhrPayload = ByteString
bytesToPayload :: BL.ByteString -> XhrPayload
bytesToPayload = BL.toStrict


performRequestsCT
    :: forall t m ct a f tag. (SupportsServantReflex t m,
        MonadHold t m,
        MimeUnrender ct a, Traversable f, ResponseData t m, RequestConfig t m, Functor (Response m))
    => Proxy ct
    -> Text
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> ClientOptions m
    -> Event t tag
    -> m (Event t (f (ReqResultRaw tag t m XhrPayload a)))
performRequestsCT ct reqMeth reqs reqHost opts trigger = do
  resps <- performRequests @XhrPayload reqMeth reqs reqHost opts trigger
  let decodeResp :: Maybe XhrResponseBody -> Either Text XhrPayload
      decodeResp (Just (XhrResponseBody_ArrayBuffer x)) = Right x
      decodeResp _ = Left "No body"
  -- first T.pack . mimeUnrender ct . BL.fromStrict
  return $ fmap
      (\(t,rs) -> ffor rs $ \r -> case r of
              Left e  -> RequestFailure t e
              Right g -> either (\le -> ResponseFailure t le g) (\ri -> ri) $ traverse (first T.pack . mimeUnrender ct . BL.fromStrict) $ evalResponse (decodeResp . (getResponseBody @t @m @XhrPayload)) (t,g)
      )
      resps


performRequestsNoBody
    :: (SupportsServantReflex t m,
        Functor (Response m),
        MonadHold t m,
        Traversable f, ResponseData t m, RequestConfig t m)
    => Text
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> ClientOptions m
    -> Event t tag
    -> m (Event t (f (ReqResultRaw tag t m XhrPayload NoContent)))
performRequestsNoBody reqMeth reqs reqHost opts trigger = do
  resps <- performRequests @() reqMeth reqs reqHost opts trigger
  let decodeResp = const $ Right NoContent
  return $ ffor resps $ \(tag,rs) -> ffor rs $ \r -> case r of
      Left e  -> RequestFailure tag e
      Right g -> evalResponse decodeResp (tag,g)


------------------------------------------------------------------------------
evalResponse
    :: forall t m a b tag. (Requester t m, ResponseData t m) => (Response m a -> Either Text b)
    -> (tag, Response m a)
    -> ReqResultRaw tag t m a b
evalResponse decode (tag, xhr) =
    let status = getStatus @t @m xhr
        okStatus = status >= 200 && status < 400
        errMsg = fromMaybe
            ("Empty response with error code " <>
                T.pack (show status))
            (getResponseText @t @m xhr)
        respPayld  = if okStatus
                     then either
                          (\e -> ResponseFailure tag e xhr)
                          (\v -> ResponseSuccess' tag v xhr)
                          (decode xhr)
                     else ResponseFailure tag errMsg xhr
    in respPayld

------------------------------------------------------------------------------
-- | Utility for simultaneously accessing/filtering Success and Failure
-- response 'Event's,
fanReqResultRaw :: Reflex t => Event t (ReqResultRaw tag t m a r) -> (Event t Text, Event t r)
fanReqResultRaw = fmapMaybe reqFailure &&& fmapMaybe reqSuccess

------------------------------------------------------------------------------
-- | Utility for simultaneously accessing/filtering Success and Failure
-- response 'Event's, but keeping the request tag
fanReqResultRaw' :: Reflex t => Event t (ReqResultRaw tag t m a r) -> (Event t (tag, Text), Event t (tag, r))
fanReqResultRaw' = fmapMaybe reqFailure' &&& fmapMaybe reqSuccess'


note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

fmapL :: (e -> e') -> Either e a -> Either e' a
fmapL _ (Right a) = Right a
fmapL f (Left e)  = Left (f e)

builderToText :: Builder.Builder -> T.Text
builderToText = TE.decodeUtf8 . BL.toStrict . Builder.toLazyByteString

escape :: T.Text -> T.Text
escape = T.pack . N.escapeURIString (not . N.isReserved) . T.unpack . TE.decodeUtf8 . BL.toStrict . Builder.toLazyByteString . toEncodedUrlPiece
