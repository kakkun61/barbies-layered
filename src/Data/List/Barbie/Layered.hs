{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.List.Barbie.Layered
  ( List (..)
  ) where

import Barbies.Bare                (Covered, Wear)
import Barbies.Bare.Layered        (BareB (bcover, bstrip))
import Data.Functor.Barbie.Layered (FunctorB (bmap))
import Data.Functor.Identity       (Identity (Identity))
import Data.Kind                   (Type)
import GHC.Generics                (Generic)

type List :: (Type -> (Type -> Type) -> Type) -> Type -> (Type -> Type) -> Type
data List a b f
  = Nil
  | Cons (Wear b f (a b f)) (Wear b f (List a b f))
  deriving Generic

instance FunctorB (a Covered) => FunctorB (List a Covered) where
  bmap :: forall f g. (Functor f, Functor g) => (forall a. f a -> g a) -> List a Covered f -> List a Covered g
  bmap _ Nil         = Nil
  bmap f (Cons x xs) = Cons (bmap f <$> f x) (bmap f <$> f xs)

instance BareB a => BareB (List a) where
  bstrip Nil                               = Nil
  bstrip (Cons (Identity x) (Identity xs)) = Cons (bstrip x) (bstrip xs)

  bcover Nil         = Nil
  bcover (Cons x xs) = Cons (Identity $ bcover x) (Identity $ bcover xs)
