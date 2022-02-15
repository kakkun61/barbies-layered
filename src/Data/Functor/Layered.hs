{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Layered
  ( FunctorB (..)
  , FoldableB (..)
  , TraversableB (..)
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

class FoldableB b where
  bfoldMap :: forall f m. (Functor f, Monoid m) => (forall a. f a -> m) -> b f -> m

instance FoldableB Proxy where
  bfoldMap _ _ = mempty

instance FoldableB Void where
  bfoldMap _ _ = mempty

instance FoldableB Unit where
  bfoldMap _ _ = mempty

instance FoldableB (Constant a) where
  bfoldMap _ (Constant _) = mempty

instance FoldableB (Const a) where
  bfoldMap _ (Const _) = mempty

instance (FoldableB a, FoldableB b) => FoldableB (Product a b) where
  bfoldMap f (Pair x y) = bfoldMap f x <> bfoldMap f y

instance (FoldableB a, FoldableB b) => FoldableB (Sum a b) where
  bfoldMap f (InL x) = bfoldMap f x
  bfoldMap f (InR x) = bfoldMap f x

instance (Foldable f, FoldableB b) => FoldableB (Compose f b) where
  bfoldMap h (Compose x) = foldMap (bfoldMap h) x

-- | Barbie-types that can be traversed from left to right. Instances should
--   satisfy the following laws:
--
-- @
-- t . 'btraverse' f = 'btraverse' (t . f) -- naturality
-- 'btraverse' 'Data.Functor.Identity' = 'Data.Functor.Identity' -- identity
-- 'btraverse' ('Compose' . 'fmap' g . f) = 'Compose' . 'fmap' ('btraverse' g) . 'btraverse' f -- composition
-- @
class (FunctorB b, FoldableB b) => TraversableB b where
  btraverse :: forall f g e. (Traversable f, Traversable g, Monad e) => (forall a. f a -> e (g a)) -> b f -> e (b g)

instance TraversableB Proxy where
  btraverse _ _ = pure Proxy

instance TraversableB Void where
  btraverse _ _ = pure undefined

instance TraversableB Unit where
  btraverse _ _ = pure Unit

instance TraversableB (Constant a) where
  btraverse _ (Constant a) = pure (Constant a)

instance TraversableB (Const a) where
  btraverse _ (Const a) = pure (Const a)

instance (TraversableB a, TraversableB b) => TraversableB (Product a b) where
  btraverse f (Pair x y) = Pair <$> btraverse f x <*> btraverse f y

instance (TraversableB a, TraversableB b) => TraversableB (Sum a b) where
  btraverse f (InL x) = InL <$> btraverse f x
  btraverse f (InR x) = InR <$> btraverse f x

instance (Traversable f, TraversableB b) => TraversableB (Compose f b) where
  btraverse f (Compose x) = Compose <$> traverse (btraverse f) x
