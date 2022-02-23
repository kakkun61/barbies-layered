{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Functor.Barbie.Layered
  ( Functor (..)
  , (<$>)
  , (<$)
  , ($>)
  ) where

import Prelude (const, flip)
import Data.Kind (Constraint, Type)
import Barbies.Layered.Functor (FunctorB)
import Barbies.Bi (Flip)
import qualified Prelude

type Functor :: ((Type -> Type) -> ((Type -> Type) -> Type) -> Type) -> Constraint
class Functor t where
  fmap :: (FunctorB (Flip t a), Prelude.Functor f) => (a f -> b f) -> t f a -> t f b

(<$>) :: (Functor t, FunctorB (Flip t a), Prelude.Functor f) => (a f -> b f) -> t f a -> t f b
(<$>) = fmap
infixl 4 <$>

(<$) :: (Functor t, FunctorB (Flip t b), Prelude.Functor f) => a f -> t f b -> t f a
a <$ b = fmap (const a) b
infixl 4 <$

($>) :: (Functor t, FunctorB (Flip t a), Prelude.Functor f) => t f a -> b f -> t f b
($>) = flip (<$)
infixl 4 $>
