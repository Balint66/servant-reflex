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
import Reflex (Reflex)
import Data.Constraint (Dict (Dict))
import Data.Kind
import Data.Proxy
import Servant.API.TypeErrors (ErrorIfNoGeneric)
import Servant.API.NamedRoutes (NamedRoutes)

type AsReflex :: Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type
data AsReflex t m f k
instance GenericMode (AsReflex t m f k) where
  type AsReflex t m f k :- api = ClientMulti t m api f k

type GReflexConstraint (api :: Type -> Type) (t :: Type) (m :: Type -> Type) (f :: Type -> Type) (k :: Type) = 
  (ToServant api (AsReflex t m f k) ~ ClientMulti t m (ToServantApi api) f k, GServantProduct (Rep (api (AsReflex t m f k))))

class GReflex (api :: Type -> Type) (t :: Type) (m :: Type -> Type) (f :: Type -> Type) (k :: Type) where
  gReflexProof :: Dict (GReflexConstraint api t m f k)

instance GReflexConstraint api t m f k => GReflex api t m f k where
  gReflexProof :: GReflexConstraint api t m f k => Dict (GReflexConstraint api t m f k)
  gReflexProof = Dict

instance (Reflex t, Applicative m, HasClientMulti t m (ToServantApi api) f k,
  forall tt mm ff kk. Generic (api (AsReflex tt mm ff kk)),
  forall tt mm ff kk. GReflex api tt mm ff kk,
  ErrorIfNoGeneric api
  ) => HasClientMulti t m (NamedRoutes api) f k where
  type ClientMulti t m (NamedRoutes api) f k = api (AsReflex t m f k)
  clientWithRouteAndResultHandlerMulti _ f m k req dyn opt nt =
    case gReflexProof @api @t @m @f @k of
        Dict -> fromServant @api @(AsReflex t m f k) $ clientWithRouteAndResultHandlerMulti (Proxy @(ToServantApi api)) f m k req dyn opt nt

