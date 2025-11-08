{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Reflex.Multi (
    -- * Compute servant client functions
    clientA
    , clientWithOptsA
    , BaseUrl(..)
    , Scheme(..)

    -- * Build QueryParam arguments
    , QParam(..)

    -- * Access response data
    , withCredentials

    -- * Access response data
    , ReqResultRaw (..)
    , ReqResult
    , reqSuccess
    , reqSuccess'
    , reqFailure
    , response

    , HasClientMulti(..)
    , BuildHeaderKeysTo(..)
    , toHeaders
    ) where

------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative     (liftA2)
#endif
import           Data.Functor.Compose    (Compose (..))
import           Data.Kind (Type)
import           Data.Proxy              (Proxy (..))
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as E
import qualified Data.CaseInsensitive    as CI
import qualified Data.Map                as Map
import           GHC.TypeLits            (KnownSymbol, symbolVal)
import           Servant.API             ((:<|>) (..), (:>), BasicAuth,
                                          BasicAuthData, BuildHeadersTo (..),
                                          Capture, Header, Headers (..),
                                          HttpVersion, IsSecure,
                                          MimeRender (..), MimeUnrender,
                                          NoContent, QueryFlag, QueryParam,
                                          QueryParams, Raw, ReflectMethod (..),
                                          RemoteHost, ReqBody,
                                          ToHttpApiData (..), Vault, Verb,
                                          contentType)
import           Servant.API.Description (Summary)

import           Reflex.Dom.Core         (Dynamic, Event, Reflex,
                                          XhrResponseHeaders (..),
                                          attachPromptlyDynWith, constDyn,
                                          Request, MonadHold(..),
                                          fmapMaybe, ffor, leftmost, Requester,
                                          Response)
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl (BaseUrl (..), Scheme (..),
                                         SupportsServantReflex, showBaseUrl)
import           Servant.Common.Req     (ClientOptions,
                                         QParam (..), QueryPart (..), Req,
                                         ReqResultRaw (..), ReqResult, addHeader, authData,
                                         defReq,
                                         defaultClientOptions,
                                         performRequestsCT,
                                         performRequestsNoBody,
                                         performSomeRequestsAsync,
                                         prependToPathParts, qParamToQueryPart,
                                         qParams, reqBody, reqFailure,
                                         reqMethod, reqSuccess, reqSuccess',
                                         respHeaders, response, withCredentials,
                                         ResponseData(..), XhrPayload, evalResponse, RequestConfig(setUrl, getUrl))


toHeaders :: forall t m ls tag a. (ResponseData t m, BuildHeadersTo ls) => ReqResult tag t m a -> ReqResult tag t m (Headers ls a)
toHeaders r =
  let hdrs = maybe []
                   (\xhr -> fmap (\(h,v) -> (CI.map E.encodeUtf8 h, E.encodeUtf8 v))
                     (Map.toList $ getResponseHeaders @t @m xhr))
                   (response r)
  in  ffor r $ \a -> Headers {getResponse = a ,getHeadersHList = buildHeadersTo hdrs}


class BuildHeaderKeysTo hs where
  buildHeaderKeysTo :: Proxy hs -> [CI.CI T.Text]

instance {-# OVERLAPPABLE #-} BuildHeaderKeysTo '[]
  where buildHeaderKeysTo _ = []

instance {-# OVERLAPPABLE #-} (BuildHeaderKeysTo xs, KnownSymbol h)
  => BuildHeaderKeysTo ((Header h v) ': xs) where
  buildHeaderKeysTo _ =
    let
      thisKey = CI.mk $ T.pack (symbolVal (Proxy :: Proxy h))
    in thisKey : buildHeaderKeysTo (Proxy :: Proxy xs)


------------------------------------------------------------------------------
clientA :: (HasClientMulti t m layout f tag, Applicative f, Reflex t)
        => Proxy layout -> Proxy m -> Proxy f -> Proxy tag
        -> Dynamic t BaseUrl -> ClientMulti t m layout f tag
clientA p q f tag baseurl  =
    clientWithRouteMulti p q f tag (constDyn (pure defReq)) baseurl
    defaultClientOptions


-- | A version of @client@ that sets the withCredentials flag
-- on requests. Use this function for clients of CORS API's
clientWithOptsA :: (HasClientMulti t m layout f tag, Applicative f, Reflex t)
                 => Proxy layout -> Proxy m -> Proxy f -> Proxy tag
                 -> Dynamic t BaseUrl -> ClientOptions m -> ClientMulti t m layout f tag
clientWithOptsA p q f tag baseurl opts =
    clientWithRouteMulti p q f tag
    (constDyn (pure defReq)) baseurl opts

------------------------------------------------------------------------------
class (Monad m) => HasClientMulti t m layout f (tag :: Type) where
  type ClientMulti t m layout f tag :: Type
  clientWithRouteMulti
    :: Proxy layout
      -> Proxy m
      -> Proxy f
      -> Proxy tag
      -> Dynamic t (f (Req t))
      -> Dynamic t BaseUrl
      -> ClientOptions m
      -> ClientMulti t m layout f tag
  clientWithRouteMulti l m f t r b o = clientWithRouteAndResultHandlerMulti l m f t r b o return

  clientWithRouteAndResultHandlerMulti
    :: Proxy layout
    -> Proxy m
    -> Proxy f
    -> Proxy tag
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> ClientOptions m
    -> (forall a. Event t (f (ReqResult tag t m a)) -> m (Event t (f (ReqResult tag t m a))))
    -> ClientMulti t m layout f tag


------------------------------------------------------------------------------
instance (HasClientMulti t m a f tag, HasClientMulti t m b f tag) =>
    HasClientMulti t m (a :<|> b) f tag where
  type ClientMulti t m (a :<|> b) f tag = ClientMulti t m a f tag :<|>
                                          ClientMulti t m b f tag
  clientWithRouteAndResultHandlerMulti Proxy q f tag reqs baseurl opts wrap =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy a) q f tag reqs baseurl opts wrap :<|>
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy b) q f tag reqs baseurl opts wrap


------------------------------------------------------------------------------
instance (SupportsServantReflex t m,
          ToHttpApiData a,
          HasClientMulti t m sublayout f tag,
          Applicative f)
      => HasClientMulti t m (Capture capture a :> sublayout) f tag where

  type ClientMulti t m (Capture capture a :> sublayout) f tag =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl opts wrap vals =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl opts wrap
    where
      reqs' = (prependToPathParts <$> ps <*>) <$> reqs
      ps    = (fmap .  fmap . fmap) toUrlPiece vals


------------------------------------------------------------------------------
-- VERB (Returning content) --
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  (MimeUnrender ct a,
   ReflectMethod method, cts' ~ (ct ': cts),
   SupportsServantReflex t m,
   Applicative f,
   Traversable f,
   MonadHold t m,
   ResponseData t m,
   RequestConfig t m,
   Functor (Response m)
  ) => HasClientMulti t m (Verb method status cts' a) f tag where

  type ClientMulti t m (Verb method status cts' a) f tag =
    Event t tag -> m (Event t (f (ReqResult tag t m a)))

  clientWithRouteAndResultHandlerMulti _ _ _ _ reqs baseurl opts wrap val =
    wrap =<< performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl opts val
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
            reqs' = fmap (\r -> r { reqMethod = method }) <$> reqs


------------------------------------------------------------------------------
-- -- VERB (No content) --
instance {-# OVERLAPPING #-}
  (ReflectMethod method,
  SupportsServantReflex t m,
  Traversable f,
  Functor (Response m),
  MonadHold t m,
  ResponseData t m,
  RequestConfig t m) =>
  HasClientMulti t m (Verb method status cts NoContent) f tag where
  type ClientMulti t m (Verb method status cts NoContent) f tag =
    Event t tag -> m (Event t (f (ReqResult tag t m NoContent)))
    -- TODO: how to access input types here?
    -- ExceptT ServantError IO NoContent
  clientWithRouteAndResultHandlerMulti Proxy _ _ _ req baseurl opts wrap val =
    wrap =<< performRequestsNoBody method req baseurl opts val
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)


------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  ( MimeUnrender ct a, BuildHeadersTo ls, BuildHeaderKeysTo ls,
    ReflectMethod method, cts' ~ (ct ': cts),
    SupportsServantReflex t m,
    Traversable f,
    MonadHold t m,
    ResponseData t m,
    RequestConfig t m,
    Functor (Response m)
  ) => HasClientMulti t m (Verb method status cts' (Headers ls a)) f tag where
  type ClientMulti t m (Verb method status cts' (Headers ls a)) f tag =
    Event t tag -> m (Event t (f (ReqResult tag t m (Headers ls a))))
  clientWithRouteAndResultHandlerMulti Proxy _ _ _ reqs baseurl opts wrap triggers = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl opts triggers :: m (Event t (f (ReqResult tag t m a)))
    wrap $ fmap toHeaders <$> resp
    where
      reqs' = fmap (\r ->
                r { respHeaders =
                    OnlyHeaders (Set.fromList
                                 (buildHeaderKeysTo (Proxy :: Proxy ls)))
                  }) <$> reqs


------------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-}
  ( BuildHeadersTo ls,
    BuildHeaderKeysTo ls,
    ReflectMethod method,
    SupportsServantReflex t m,
    Traversable f,
    Functor (Response m),
    MonadHold t m,
    ResponseData t m,
    RequestConfig t m
  ) => HasClientMulti t m (Verb method status
                           cts (Headers ls NoContent)) f tag where
  type ClientMulti t m (Verb method status cts (Headers ls NoContent)) f tag
    = Event t tag -> m (Event t (f (ReqResult tag t m (Headers ls NoContent))))
  clientWithRouteAndResultHandlerMulti Proxy _ _ _ reqs baseurl opts wrap triggers = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsNoBody method reqs' baseurl opts triggers
    wrap $ fmap toHeaders <$> resp
    where reqs' = fmap (\req ->
                    req {respHeaders = OnlyHeaders (Set.fromList
                         (buildHeaderKeysTo (Proxy :: Proxy ls)))
                        }) <$> reqs


------------------------------------------------------------------------------
instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f tag,
          SupportsServantReflex t m,
          Traversable f,
          Applicative f)
      => HasClientMulti t m (Header sym a :> sublayout) f tag where

  type ClientMulti t m (Header sym a :> sublayout) f tag =
    f (Dynamic t (Either Text a)) -> ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti Proxy f q tag reqs baseurl opts wrap eVals =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) f
                    q tag
                    reqs'
                    baseurl opts wrap
    where hname = T.pack $ symbolVal (Proxy :: Proxy sym)
          reqs' = ((\eVal req -> Servant.Common.Req.addHeader hname eVal req)
                  <$> eVals <*>) <$> reqs


------------------------------------------------------------------------------
instance HasClientMulti t m sublayout f tag
  => HasClientMulti t m (HttpVersion :> sublayout) f tag where

  type ClientMulti t m (HttpVersion :> sublayout) f tag =
    ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti Proxy q f tag =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag


------------------------------------------------------------------------------
instance (HasClientMulti t m sublayout f tag, KnownSymbol sym)
  => HasClientMulti t m (Summary sym :> sublayout) f tag where

  type ClientMulti t m (Summary sym :> sublayout) f tag =
    ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti Proxy q f tag =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag


------------------------------------------------------------------------------
instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Applicative f)
      => HasClientMulti t m (QueryParam sym a :> sublayout) f tag where

  type ClientMulti t m (QueryParam sym a :> sublayout) f tag =
    Dynamic t (f (QParam a)) -> ClientMulti t m sublayout f tag

  -- if mparam = Nothing, we don't add it to the query string
  -- TODO: Check the above comment
  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl opts wrap mparams =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag
      reqs' baseurl opts wrap

    where pname = symbolVal (Proxy :: Proxy sym)
          p prm = QueryPartParam $ fmap qParamToQueryPart prm
          paramPair mp = (T.pack pname, p mp)
          -- reqs' = (\params reqs -> (\req param -> req {qParams = paramPair param : qParams req}) <$> reqs <*> params)
          --         <$> mparams <*> reqs
          reqs' = liftA2 (\(pr :: QParam a) (r :: Req t) -> r { qParams = paramPair (constDyn pr) : qParams r })
                  <$> mparams <*> reqs


instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Applicative f)
      => HasClientMulti t m (QueryParams sym a :> sublayout) f tag where

  type ClientMulti t m (QueryParams sym a :> sublayout) f tag =
    Dynamic t (f [a]) -> ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl opts wrap paramlists =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl opts wrap

      where req' l r = r { qParams =  (T.pack pname, params' (constDyn l)) : qParams r }
            pname   = symbolVal (Proxy :: Proxy sym)
            params' l = QueryPartParams $ (fmap . fmap) (toQueryParam)
                        l
            reqs' = liftA2 req' <$> paramlists <*> reqs


instance (KnownSymbol sym,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Applicative f)
      => HasClientMulti t m (QueryFlag sym :> sublayout) f tag where

  type ClientMulti t m (QueryFlag sym :> sublayout) f tag =
    Dynamic t (f Bool) -> ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti _ q f' tag reqs baseurl opts wrap flags =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f' tag reqs' baseurl opts wrap

    where req' f req = req { qParams = thisPair (constDyn f) : qParams req }
          thisPair f = (T.pack pName, QueryPartFlag f) :: (Text, QueryPart t)
          pName      = symbolVal (Proxy :: Proxy sym)
          reqs'      = liftA2 req' <$> flags <*> reqs


instance (SupportsServantReflex t m, Requester t m, ResponseData t m,
          Traversable f, Applicative f, MonadHold t m, RequestConfig t m) => HasClientMulti t m Raw f tag where
  type ClientMulti t m Raw f tag = f (Dynamic t (Either Text (Request m XhrPayload)))
                                 -> Event t tag
                                 -> m (Event t (f (ReqResult tag t m ())))
  
  clientWithRouteAndResultHandlerMulti _ _ _ _ _ baseurl opts wrap rawReqs triggers = do
    let rawReqs' = sequence $ fmap (\t ->liftA2 (\x path -> case x of
                             Left e -> Left e
                             Right jx -> Right $ setUrl @t @m jx (path <> getUrl @t @m jx)
                         )  t (showBaseUrl <$> baseurl)) rawReqs
        rawReqs'' = attachPromptlyDynWith (\fxhr t -> Compose (t, fxhr)) rawReqs' triggers
    resps' <- {-fmap (fmap (evalResponse (const $ Right ())) . sequenceA . getCompose)-} performSomeRequestsAsync opts rawReqs''
    let 
        badReq = fmapMaybe (\(Compose (t, fx)) -> traverse (either (Just . (t,)) (const Nothing)) fx) resps'
        okReq  = fmapMaybe (\(Compose (t , fx)) -> traverse(either (const Nothing) (Just . (t,))) fx) resps'
    let resps = fmap (evalResponse (const $ Right ())) <$> okReq
    let
        ret :: Event t (f (ReqResult tag t m ()))
        ret = leftmost [ fmap (fmap (uncurry RequestFailure)) badReq
                      , resps
                      ]
    wrap ret


instance (MimeRender ct a,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Applicative f)
      => HasClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f tag where

  type ClientMulti t m (ReqBody (ct ': cts) a :> sublayout) f tag =
    Dynamic t (f (Either Text a)) -> ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti Proxy q f tag reqs baseurl opts wrap bodies =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag reqs' baseurl opts wrap
       where req'        b r = r { reqBody = bodyBytesCT (constDyn b) }
             ctProxy         = Proxy :: Proxy ct
             ctString        = T.pack $ show $ contentType ctProxy
             bodyBytesCT b   = Just $ (fmap . fmap)
                               (\b' -> (mimeRender ctProxy b', ctString))
                               b
             reqs'           = liftA2 req' <$> bodies <*> reqs


instance (KnownSymbol path,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Functor f) => HasClientMulti t m (path :> sublayout) f tag where
  type ClientMulti t m (path :> sublayout) f tag = ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl =
     clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag
                     (fmap (prependToPathParts (pure (Right $ T.pack p))) <$> reqs)
                     baseurl

    where p = symbolVal (Proxy :: Proxy path)


instance HasClientMulti t m api f tag => HasClientMulti t m (Vault :> api) f tag where
  type ClientMulti t m (Vault :> api) f tag = ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api) q f tag reqs baseurl


instance HasClientMulti t m api f tag => HasClientMulti t m (RemoteHost :> api) f tag where
  type ClientMulti t m (RemoteHost :> api) f tag = ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api) q f tag reqs baseurl


instance HasClientMulti t m api f tag => HasClientMulti t m (IsSecure :> api) f tag where
  type ClientMulti t m (IsSecure :> api) f tag = ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api) q f tag reqs baseurl


instance (HasClientMulti t m api f tag, Reflex t, Applicative f)
      => HasClientMulti t m (BasicAuth realm usr :> api) f tag where

  type ClientMulti t m (BasicAuth realm usr :> api) f tag = Dynamic t (f (Maybe BasicAuthData))
                                               -> ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl opts wrap authdatas =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api) q f tag reqs' baseurl opts wrap
      where
        req'  a r = r { authData = Just (constDyn a) }
        reqs' = liftA2 req' <$> authdatas <*> reqs
