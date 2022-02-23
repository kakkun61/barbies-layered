{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Barbies.Layered.Traversable
  ( TraversableB (..)
  ) where

import Barbies                 (Unit (Unit), Void)
import Control.Category.Const2 (Const2 (Const2))
import Data.Data               (Proxy (Proxy))
import Data.Functor.Compose    (Compose (Compose))
import Data.Functor.Const      (Const (Const))
import Data.Functor.Constant   (Constant (Constant))
import Data.Functor.Product    (Product (Pair))
import Data.Functor.Sum        (Sum (InL, InR))
import Prelude (Traversable (traverse), Monad, Applicative (pure, (<*>)), undefined, (<$>))
import Barbies.Layered.Functor (FunctorB)
import Barbies.Layered.Foldable (FoldableB)

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

instance TraversableB (Const2 a b) where
  btraverse _ (Const2 a) = pure (Const2 a)
