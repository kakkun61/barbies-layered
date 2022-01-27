{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Maybe.Barbie.Layered
  ( Maybe (..)
  ) where

import Barbies.Bare                (Covered, Wear)
import Barbies.Bare.Layered        (BareB (bcover, bstrip))
import Data.Functor.Barbie.Layered (FunctorB (bmap))
import Data.Functor.Identity       (Identity (Identity))
import Data.Kind                   (Type)
import GHC.Generics                (Generic)
import Prelude                     (Functor, ($), (<$>))

type Maybe :: (Type -> (Type -> Type) -> Type) -> Type -> (Type -> Type) -> Type
data Maybe a b f
  = Nothing
  | Just (Wear b f (a b f))
  deriving Generic

instance FunctorB (a Covered) => FunctorB (Maybe a Covered) where
  bmap :: forall f g. (Functor f, Functor g) => (forall a. f a -> g a) -> Maybe a Covered f -> Maybe a Covered g
  bmap _ Nothing  = Nothing
  bmap f (Just x) = Just $ bmap f <$> f x

instance BareB a => BareB (Maybe a) where
  bstrip Nothing             = Nothing
  bstrip (Just (Identity x)) = Just $ bstrip x

  bcover Nothing  = Nothing
  bcover (Just x) = Just $ Identity $ bcover x
