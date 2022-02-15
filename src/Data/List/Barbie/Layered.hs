{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Data.List.Barbie.Layered
  ( List (..)
  ) where

import Data.Barbie.Layered   (IsBarbie (Bare, pullOff, putOn))
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Layered  (FunctorB (bmap), TraversableB (btraverse), FoldableB (bfoldMap))
import Data.Kind             (Type)
import GHC.Generics          (Generic)

type List :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data List a f
  = Nil
  | Cons (f (a f)) (f (List a f))
  deriving Generic

deriving instance (Show (f (a f)), Show (f (List a f))) => Show (List a f)
deriving instance (Read (f (a f)), Read (f (List a f))) => Read (List a f)
deriving instance (Eq (f (a f)), Eq (f (List a f))) => Eq (List a f)
deriving instance (Ord (f (a f)), Ord (f (List a f))) => Ord (List a f)

instance IsBarbie a => IsBarbie (List a) where
  type Bare (List a) = [Bare a]

  pullOff Nil                               = []
  pullOff (Cons (Identity x) (Identity xs)) = pullOff x : pullOff xs

  putOn []       = Nil
  putOn (x : xs) = Cons (Identity $ putOn x) (Identity $ putOn xs)

instance FunctorB a => FunctorB (List a) where
  bmap _ Nil         = Nil
  bmap f (Cons x xs) = Cons (bmap f <$> f x) (bmap f <$> f xs)

instance FoldableB a => FoldableB (List a) where
  bfoldMap _ Nil = mempty
  bfoldMap f (Cons x xs) = f (bfoldMap f <$> x) <> f (bfoldMap f <$> xs)

instance TraversableB a => TraversableB (List a) where
  btraverse _ Nil = pure Nil
  btraverse f (Cons x xs) = do
    x' <- f x
    x'' <- sequenceA $ btraverse f <$> x'
    xs' <- f xs
    xs'' <- sequenceA $ btraverse f <$> xs'
    pure $ Cons x'' xs''
