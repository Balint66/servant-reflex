{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
import           Servant.API.Capture     (CaptureAll)
import           Servant.API.Description (Summary)

import           Reflex.Dom.Core         (Dynamic, Event, Reflex,
                                          XhrResponseHeaders (..),
                                          attachPromptlyDynWith, constDyn,
                                          fmapMaybe, ffor, leftmost,
                                          XhrResponse (_xhrResponse_headers), XhrRequest)
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl (BaseUrl (..), Scheme (..),
                                         SupportsServantReflex)
import           Servant.Common.Req     (ClientOptions,
                                         QParam (..), QueryPart (..), Req,
                                         ReqResult(..), addHeader, authData,
                                         defReq,
                                         defaultClientOptions,
                                         performRequestsCT,
                                         performRequestsNoBody,
                                         performSomeRequestsAsync,
                                         prependToPathParts, qParamToQueryPart,
                                         qParams, reqBody, reqFailure,
                                         reqMethod, reqSuccess, reqSuccess',
                                         respHeaders, response, withCredentials,
                                         evalResponse)


toHeaders :: forall ls tag a. (BuildHeadersTo ls) => ReqResult tag a -> ReqResult tag (Headers ls a)
toHeaders r =
  let hdrs = maybe []
                    (\xhr -> fmap (\(h,v) -> (CI.map E.encodeUtf8 h, E.encodeUtf8 v))
                      (Map.toList $ _xhrResponse_headers xhr))
                    (response r)
  in  ffor r $ \a -> Headers {getResponse = a ,getHeadersHList = buildHeadersTo hdrs}

class BuildHeaderKeysTo hs where
  buildHeaderKeysTo :: Proxy hs -> [CI.CI T.Text]

instance {-# OVERLAPPABLE #-} BuildHeaderKeysTo '[]
  where buildHeaderKeysTo _ = []

instance {-# OVERLAPPABLE #-} (BuildHeaderKeysTo xs, KnownSymbol h)
  => BuildHeaderKeysTo (Header h v ': xs) where
  buildHeaderKeysTo _ =
    let
      thisKey = CI.mk . T.pack . symbolVal $ (Proxy :: Proxy h)
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
                  -> Dynamic t BaseUrl -> ClientOptions -> ClientMulti t m layout f tag
clientWithOptsA p q f tag =
    clientWithRouteMulti p q f tag .
    constDyn . pure $ defReq

------------------------------------------------------------------------------
class (Applicative m) => HasClientMulti t m layout f (tag :: Type) where
  type ClientMulti t m layout f tag :: Type
  clientWithRouteMulti
    :: Proxy layout
      -> Proxy m
      -> Proxy f
      -> Proxy tag
      -> Dynamic t (f (Req t))
      -> Dynamic t BaseUrl
      -> ClientOptions
      -> ClientMulti t m layout f tag
  clientWithRouteMulti l m f t r b o = clientWithRouteAndResultHandlerMulti l m f t r b o pure

  clientWithRouteAndResultHandlerMulti
    :: Proxy layout
    -> Proxy m
    -> Proxy f
    -> Proxy tag
    -> Dynamic t (f (Req t))
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> (forall a. Event t (f (ReqResult tag a)) -> m (Event t (f (ReqResult tag a))))
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
-- Capture. Example:
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > myApi :: Proxy MyApi = Proxy
-- >
-- > getBook :: SupportsServantReflex t m
--           => Dynamic t BaseUrl
--           -> Dynamic t (Maybe Text)
--           -> Event t l
--           -> m (Event t (l, ReqResult Book))
-- > getBook = client myApi (constDyn host)
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
   Traversable f
  ) => HasClientMulti t m (Verb method status cts' a) f tag where

  type ClientMulti t m (Verb method status cts' a) f tag =
    Event t tag -> m (Event t (f (ReqResult tag a)))

  clientWithRouteAndResultHandlerMulti _ _ _ _ reqs baseurl opts wrap val =
    wrap =<< performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl opts val
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
            reqs' = fmap (\r -> r { reqMethod = method }) <$> reqs


------------------------------------------------------------------------------
-- -- VERB (No content) --
instance {-# OVERLAPPING #-}
  (ReflectMethod method,
  SupportsServantReflex t m,
  Traversable f
  ) =>
  HasClientMulti t m (Verb method status cts NoContent) f tag where
  type ClientMulti t m (Verb method status cts NoContent) f tag =
    Event t tag -> m (Event t (f (ReqResult tag NoContent)))
    -- TODO: how to access input types here?
    -- ExceptT ServantError IO NoContent
  clientWithRouteAndResultHandlerMulti Proxy _ _ _ req baseurl opts wrap val =
    wrap =<< performRequestsNoBody method req baseurl opts val
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)


------------------------------------------------------------------------------
-- HEADERS Verb (Content) --
-- Headers combinator not treated in fully general case,
-- in order to deny instances for (Headers ls (Capture "id" Int)),
-- a combinator that wouldn't make sense
instance {-# OVERLAPPABLE #-}
  -- Note [Non-Empty Content Types]
  ( MimeUnrender ct a, BuildHeadersTo ls, BuildHeaderKeysTo ls,
    ReflectMethod method, cts' ~ (ct ': cts),
    SupportsServantReflex t m,
    Traversable f
  ) => HasClientMulti t m (Verb method status cts' (Headers ls a)) f tag where
  type ClientMulti t m (Verb method status cts' (Headers ls a)) f tag =
    Event t tag -> m (Event t (f (ReqResult tag (Headers ls a))))
  clientWithRouteAndResultHandlerMulti Proxy _ _ _ reqs baseurl opts wrap triggers = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsCT (Proxy :: Proxy ct) method reqs' baseurl opts triggers :: m (Event t (f (ReqResult tag a)))
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
    Traversable f
  ) => HasClientMulti t m (Verb method status
                          cts (Headers ls NoContent)) f tag where
  type ClientMulti t m (Verb method status cts (Headers ls NoContent)) f tag
    = Event t tag -> m (Event t (f (ReqResult tag (Headers ls NoContent))))
  clientWithRouteAndResultHandlerMulti Proxy _ _ _ reqs baseurl opts wrap triggers = do
    let method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy method)
    resp <- performRequestsNoBody method reqs' baseurl opts triggers
    wrap $ fmap toHeaders <$> resp
    where reqs' = fmap (\req ->
                    req {respHeaders = OnlyHeaders (Set.fromList
                          (buildHeaderKeysTo (Proxy :: Proxy ls)))
                        }) <$> reqs


------------------------------------------------------------------------------
-- HEADER
-- > newtype Referer = Referer { referrer :: Text }
-- >   deriving (Eq, Show, Generic, FromText, ToHttpApiData)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- >
-- > viewReferer :: Maybe Referer -> ExceptT String IO Book
-- > viewReferer = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "viewRefer" to query that endpoint
-- > -- specifying Nothing or e.g Just "http://haskell.org/" as arguments
instance (KnownSymbol sym,
          ToHttpApiData a,
          HasClientMulti t m sublayout f tag,
          SupportsServantReflex t m,
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
          reqs' = (Servant.Common.Req.addHeader hname
                  <$> eVals <*>) <$> reqs


------------------------------------------------------------------------------
-- | Using a 'HttpVersion' combinator in your API doesn't affect the client
-- functions.
instance HasClientMulti t m sublayout f tag
  => HasClientMulti t m (HttpVersion :> sublayout) f tag where

  type ClientMulti t m (HttpVersion :> sublayout) f tag =
    ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti _ =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout)


------------------------------------------------------------------------------
-- | Using a 'Summary' combinator in your API doesn't affect the client
-- functions.
instance (HasClientMulti t m sublayout f tag, KnownSymbol sym)
  => HasClientMulti t m (Summary sym :> sublayout) f tag where

  type ClientMulti t m (Summary sym :> sublayout) f tag =
    ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti _ =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout)


------------------------------------------------------------------------------
-- | If you use a 'QueryParam' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'QueryParam',
-- enclosed in Maybe.
--
-- If you give Nothing, nothing will be added to the query string.
--
-- If you give a non-'Nothing' value, this function will take care
-- of inserting a textual representation of this value in the query string.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToHttpApiData' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: Maybe Text -> ExceptT String IO [Book]
-- > getBooksBy = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
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

-- | If you use a 'QueryParams' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument, a list of values of the type specified
-- by your 'QueryParams'.
--
-- If you give an empty list, nothing will be added to the query string.
--
-- Otherwise, this function will take care
-- of inserting a textual representation of your values in the query string,
-- under the same query string parameter name.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToHttpApiData' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: [Text] -> ExceptT String IO [Book]
-- > getBooksBy = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
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

-- | If you use a 'QueryFlag' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional 'Bool' argument.
--
-- If you give 'False', nothing will be added to the query string.
--
-- Otherwise, this function will insert a value-less query string
-- parameter under the name associated to your 'QueryFlag'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooks :: Bool -> ExceptT String IO [Book]
-- > getBooks = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books
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

-- | Send a raw 'XhrRequest ()' directly to 'baseurl'
instance (SupportsServantReflex t m,
  Traversable f,
  Applicative f
  ) => HasClientMulti t m Raw f tag where
  type ClientMulti t m Raw f tag = f (Dynamic t (Either Text (XhrRequest ())))
                                 -> Event t tag
                                 -> m (Event t (f (ReqResult tag ())))

  clientWithRouteAndResultHandlerMulti _ _ _ _ _ _ opts wrap rawReqs triggers = do
    let rawReqs' = sequence rawReqs :: Dynamic t (f (Either Text (XhrRequest ())))
        rawReqs'' = attachPromptlyDynWith (\fxhr t -> Compose (t, fxhr)) rawReqs' triggers
        aux (tag, Right r) = Right (tag, r)
        aux (tag, Left  e) = Left (tag, e)
    resps' <- fmap (fmap aux . sequenceA . getCompose) <$> performSomeRequestsAsync opts rawReqs''
    let
        badReq = {- fmapMaybe (\(Compose (t, fx)) -> traverse (either (Just . (t,)) (const Nothing)) fx) $ -}
                  fmapMaybe (traverse (either Just (const Nothing))) $ resps'
        okReq  = {- fmapMaybe (\(Compose (t , fx)) -> traverse(either (const Nothing) (Just . (t,))) fx) $ -}
                    fmapMaybe (traverse (either (const Nothing) Just)) $ resps'
    let resps = fmap (evalResponse (const $ Right ())) <$> okReq
    let
        ret :: Event t (f (ReqResult tag ()))
        ret = leftmost [ fmap (fmap (uncurry RequestFailure)) badReq
                      , resps
                      ]
    wrap ret

-- | If you use a 'ReqBody' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'ReqBody'.
-- That function will take care of encoding this argument as JSON and
-- of using it as the request body.
--
-- All you need is for your type to have a 'ToJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > addBook :: Book -> ExceptT String IO Book
-- > addBook = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
-- > -- then you can just use "addBook" to query that endpoint
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
            ctString        = T.pack . show . contentType $ ctProxy
            bodyBytesCT b   = Just $ (fmap . fmap)
                              (\b' -> (mimeRender ctProxy b', ctString))
                              b
            reqs'           = liftA2 req' <$> bodies <*> reqs

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path,
          HasClientMulti t m sublayout f tag,
          Reflex t,
          Functor f) => HasClientMulti t m (path :> sublayout) f tag where
  type ClientMulti t m (path :> sublayout) f tag = ClientMulti t m sublayout f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs =
     clientWithRouteAndResultHandlerMulti (Proxy :: Proxy sublayout) q f tag
                     (fmap (prependToPathParts (pure . Right . T.pack $ p)) <$> reqs)

    where p = symbolVal (Proxy :: Proxy path)


instance HasClientMulti t m api f tag => HasClientMulti t m (Vault :> api) f tag where
  type ClientMulti t m (Vault :> api) f tag = ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api)


instance HasClientMulti t m api f tag => HasClientMulti t m (RemoteHost :> api) f tag where
  type ClientMulti t m (RemoteHost :> api) f tag = ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api)


instance HasClientMulti t m api f tag => HasClientMulti t m (IsSecure :> api) f tag where
  type ClientMulti t m (IsSecure :> api) f tag = ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api)


instance (HasClientMulti t m api f tag, Reflex t, Applicative f)
      => HasClientMulti t m (BasicAuth realm usr :> api) f tag where

  type ClientMulti t m (BasicAuth realm usr :> api) f tag = Dynamic t (f (Maybe BasicAuthData))
                                               -> ClientMulti t m api f tag

  clientWithRouteAndResultHandlerMulti _ q f tag reqs baseurl opts wrap authdatas =
    clientWithRouteAndResultHandlerMulti (Proxy :: Proxy api) q f tag reqs' baseurl opts wrap
      where
        req'  a r = r { authData = Just (constDyn a) }
        reqs' = liftA2 req' <$> authdatas <*> reqs

instance (Functor f, SupportsServantReflex t m, HasClientMulti t m subLayout f tag, ToHttpApiData ty) => HasClientMulti t m (CaptureAll symb ty :> subLayout) f tag where
  type ClientMulti t m (CaptureAll symb ty :> subLayout) f tag =
    Dynamic t (Either Text ty) -> ClientMulti t m subLayout f tag
  clientWithRouteAndResultHandlerMulti _ f q t req baseurl opts warp trigs = let
    p = (fmap . fmap) toUrlPiece trigs
    in clientWithRouteAndResultHandlerMulti (Proxy @subLayout) f q t (fmap (fmap (prependToPathParts p)) req) baseurl opts warp

{- Note [Non-Empty Content Types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Rather than have

   instance (..., cts' ~ (ct ': cts)) => ... cts' ...

It may seem to make more sense to have:

   instance (...) => ... (ct ': cts) ...

But this means that if another instance exists that does *not* require
non-empty lists, but is otherwise more specific, no instance will be overall
more specific. This in turn generally means adding yet another instance (one
for empty and one for non-empty lists).
-}
