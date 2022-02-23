{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Barbies.Layered.Foldable
  ( FoldableB (..)
  ) where

import Barbies                 (Void, Unit)
import Control.Category.Const2 (Const2 (Const2))
import Data.Data               (Proxy ())
import Data.Functor.Compose    (Compose (Compose))
import Data.Functor.Const      (Const ())
import Data.Functor.Constant   (Constant ())
import Data.Functor.Product    (Product (Pair))
import Data.Functor.Sum        (Sum (InL, InR))
import Prelude (Monoid (mempty), Foldable (foldMap), Semigroup ((<>)))
import qualified Prelude

class FoldableB b where
  bfoldMap :: forall f m. (Prelude.Functor f, Monoid m) => (forall a. f a -> m) -> b f -> m

instance FoldableB Proxy where
  bfoldMap _ _ = mempty

instance FoldableB Void where
  bfoldMap _ _ = mempty

instance FoldableB Unit where
  bfoldMap _ _ = mempty

instance FoldableB (Constant a) where
  bfoldMap _ _ = mempty

instance FoldableB (Const a) where
  bfoldMap _ _ = mempty

instance (FoldableB a, FoldableB b) => FoldableB (Product a b) where
  bfoldMap f (Pair x y) = bfoldMap f x <> bfoldMap f y

instance (FoldableB a, FoldableB b) => FoldableB (Sum a b) where
  bfoldMap f (InL x) = bfoldMap f x
  bfoldMap f (InR x) = bfoldMap f x

instance (Foldable f, FoldableB b) => FoldableB (Compose f b) where
  bfoldMap h (Compose x) = foldMap (bfoldMap h) x

instance FoldableB (Const2 a b) where
  bfoldMap _ _ = mempty
