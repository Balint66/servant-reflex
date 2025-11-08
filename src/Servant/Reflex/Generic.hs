{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Servant.Reflex.Generic where
import Servant.API.Generic (GenericMode(type (:-)), ToServant, GServantProduct, Generic (Rep), ToServantApi, fromServant)
import Servant.Reflex.Multi (HasClientMulti(..))
import Servant.Common.BaseUrl (SupportsServantReflex)
import Servant.API (ToHttpApiData (toUrlPiece), (:>))
import Servant.API.Capture (CaptureAll)
import Reflex (Reflex(Dynamic))
import Data.Constraint (Dict (Dict))
import Data.Kind
import           Data.Text               (Text)
import Data.Proxy
import Servant.API.TypeErrors (ErrorIfNoGeneric)
import Servant.API.NamedRoutes (NamedRoutes)
import Servant.Common.Req (prependToPathParts)

type AsReflex :: Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type
data AsReflex t m f k
instance GenericMode (AsReflex t m f k) where
    type AsReflex t m f k :- api = ClientMulti t m api f k

instance (Functor f, SupportsServantReflex t m, HasClientMulti t m subLayout f tag, ToHttpApiData ty) => HasClientMulti t m (CaptureAll symb ty :> subLayout) f tag where
  type ClientMulti t m (CaptureAll symb ty :> subLayout) f tag =
    Dynamic t (Either Text ty) -> ClientMulti t m subLayout f tag
  clientWithRouteAndResultHandlerMulti _ f q t req baseurl opts warp trigs = let
        p = ((fmap . fmap) toUrlPiece trigs)
    in clientWithRouteAndResultHandlerMulti (Proxy @subLayout) f q t (fmap (fmap (prependToPathParts p)) req) baseurl opts warp

type GReflexConstraint (api :: Type -> Type) (t :: Type) (m :: Type -> Type) (f :: Type -> Type) (k :: Type) = 
    (ToServant api (AsReflex t m f k) ~ ClientMulti t m (ToServantApi api) f k, GServantProduct (Rep (api (AsReflex t m f k))))

class GReflex (api :: Type -> Type) (t :: Type) (m :: Type -> Type) (f :: Type -> Type) (k :: Type) where
    gReflexProof :: Dict (GReflexConstraint api t m f k)

instance GReflexConstraint api t m f k => GReflex api t m f k where
    gReflexProof :: GReflexConstraint api t m f k => Dict (GReflexConstraint api t m f k)
    gReflexProof = Dict

instance (Monad m, HasClientMulti t m (ToServantApi api) f k,
        forall tt mm ff kk. Generic (api (AsReflex tt mm ff kk)),
        forall tt mm ff kk. GReflex api tt mm ff kk,
--        GReflexConstraint api t m k,
        ErrorIfNoGeneric api
        ) => HasClientMulti t m (NamedRoutes api) f k where
        type ClientMulti t m (NamedRoutes api) f k = api (AsReflex t m f k)
        clientWithRouteAndResultHandlerMulti _ f m k req dyn opt nt =
            case gReflexProof @api @t @m @f @k of
                Dict -> fromServant @api @(AsReflex t m f k) $ clientWithRouteAndResultHandlerMulti (Proxy @(ToServantApi api)) f m k req dyn opt nt

