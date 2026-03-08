{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

-- #include "overlapping-compat.h"
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Reflex
  ( client
  , clientWithOpts
  , clientWithOptsAndResultHandler
  , clientWithRoute
  , clientWithRouteAndResultHandler
  , BuildHeaderKeysTo(..)
  , toHeaders
  , HasClient
  , Client
  , module Servant.Common.Req
  , module Servant.Common.BaseUrl
  ) where

------------------------------------------------------------------------------
import           Data.Functor.Identity
import Data.Kind
import           Data.Proxy              (Proxy (..))
import           Reflex.Dom.Core         (Dynamic, Event, Reflex, constDyn,
                                          )
------------------------------------------------------------------------------
import           Servant.Common.BaseUrl  (BaseUrl(..), Scheme(..), baseUrlWidget,
                                          showBaseUrl,
                                          SupportsServantReflex)
import           Servant.Common.Req      (ClientOptions(..),
                                          defaultClientOptions,
                                          Req, ReqResult(..), QParam(..),
                                          QueryPart(..), addHeader, authData,
                                          defReq, evalResponse, prependToPathParts,
                                          XhrPayload,
                                          performRequestsCT,
                                          performRequestsNoBody,
                                          performSomeRequestsAsync,
                                          qParamToQueryPart, reqBody,
                                          reqSuccess, reqFailure,
                                          reqMethod, respHeaders,
                                          response,
                                          reqTag,
                                          qParams, withCredentials)
import Servant.Reflex.Multi (BuildHeaderKeysTo(..),
    toHeaders,
    HasClientMulti(..))

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: Event t l -> m (Event t (l, ReqResult [Book]))
-- > postNewBook :: Dynamic t (Maybe Book) -> Event t l
-- >             -> m (Event t (l, ReqResult Book)))
-- > (getAllBooks :<|> postNewBook) = client myApi host
-- >   where host = constDyn $ BaseUrl Http "localhost" 8080
client
    :: (HasClient t m layout tag, Reflex t)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> Client t m layout tag
client p q t baseurl = clientWithRoute p q t (constDyn defReq) baseurl defaultClientOptions

clientWithOpts
    :: (HasClient t m layout tag, Reflex t)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> Client t m layout tag
clientWithOpts p q t = clientWithRoute p q t (constDyn defReq)

-- | Like 'clientWithOpts' but allows passing a function which will process the
-- result event in some way. This can be used to handle errors in a uniform way
-- across call sites.
clientWithOptsAndResultHandler
    :: (HasClient t m layout tag, Reflex t)
    => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> (forall a. Event t (ReqResult tag a) -> m (Event t (ReqResult tag a)))
    -> Client t m layout tag
clientWithOptsAndResultHandler p q t = clientWithRouteAndResultHandler p q t (constDyn defReq)


type Client t m layout tag = ClientMulti t m layout Identity tag
-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
type HasClient t m layout (tag :: Type) = HasClientMulti t m layout Identity tag
--class (HasClientMulti t m layout Identity tag) => HasClient t m layout (tag :: Type) where
clientWithRoute
    :: (HasClient t m layout tag, Reflex t) => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t (Req t)
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> Client t m layout tag
clientWithRoute l m t r b o = clientWithRouteAndResultHandler l m t r b o pure

clientWithRouteAndResultHandler
    :: (HasClient t m layout tag, Reflex t) => Proxy layout
    -> Proxy m
    -> Proxy tag
    -> Dynamic t (Req t)
    -> Dynamic t BaseUrl
    -> ClientOptions
    -> (forall a. Event t (ReqResult tag a) -> m (Event t (ReqResult tag a)))
    -> Client t m layout tag
clientWithRouteAndResultHandler l m t r url opt wr = clientWithRouteAndResultHandlerMulti l m (Proxy :: Proxy Identity) t (fmap pure r) url opt (fmap (fmap Identity) . wr . fmap runIdentity)


{-
-- SUPPORT FOR servant-auth --

-- For JavaScript clients we should be sending/storing JSON web tokens in a
-- way that is inaccessible to JavaScript.
--
-- For @servant-auth@ this is done with HTTP-only cookies. In a Reflex-DOM
-- app this means the @servant-auth@ client should only verify that the API
-- supports Cookie-based authentication but do nothing with the token
-- directly.

-- @HasCookieAuth auths@ is nominally a redundant constraint, but ensures
-- we're not trying to rely on cookies when the API does not use them.
instance (HasCookieAuth auths, HasClient t m api tag) => HasClient t m (Auth.Auth auths a :> api) tag where

  type Client t m (Auth.Auth auths a :> api) tag = Client t m api tag
  clientWithRouteAndResultHandler Proxy = clientWithRouteAndResultHandler (Proxy :: Proxy api)


type family HasCookieAuth xs :: Constraint where
  HasCookieAuth (Auth.Cookie ': xs) = ()
  HasCookieAuth (x ': xs)   = HasCookieAuth xs
  HasCookieAuth '[]         = CookieAuthNotEnabled

class CookieAuthNotEnabled
-}
