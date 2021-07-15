{-# LANGUAGE RankNTypes               #-}

module Data.Functor.Barbie.Layered
  ( FunctorB (..)
  ) where

import Barbies               (Unit (Unit), Void)
import Data.Data             (Proxy (Proxy))
import Data.Functor.Compose  (Compose (Compose))
import Data.Functor.Const    (Const (Const))
import Data.Functor.Constant (Constant (Constant))
import Data.Functor.Product  (Product (Pair))
import Data.Functor.Sum      (Sum (InL, InR))

-- | Barbie-types that can be mapped over. Instances of 'FunctorB' should
-- satisfy the following laws:
--
-- @
-- 'bmap' 'id' = 'id'
-- 'bmap' f . 'bmap' g = 'bmap' (f . g)
-- @
class FunctorB b where
  bmap :: forall f g. (Functor f, Functor g) => (forall a. f a -> g a) -> b f -> b g

instance FunctorB Proxy where
  bmap _ _ = Proxy

instance FunctorB Void where
  bmap _ _ = undefined

instance FunctorB Unit where
  bmap _ _ = Unit

instance FunctorB (Constant a) where
  bmap _ (Constant a) = Constant a

instance FunctorB (Const a) where
  bmap _ (Const a) = Const a

instance (FunctorB a, FunctorB b) => FunctorB (Product a b) where
  bmap f (Pair x y) = Pair (bmap f x) (bmap f y)

instance (FunctorB a, FunctorB b) => FunctorB (Sum a b) where
  bmap f (InL x) = InL (bmap f x)
  bmap f (InR x) = InR (bmap f x)

instance (Functor f, FunctorB b) => FunctorB (Compose f b) where
  bmap h (Compose x) = Compose (bmap h <$> x)