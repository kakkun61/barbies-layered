{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE UndecidableInstances     #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
#endif

module Data.List.Barbie.Layered
  ( List (..)
  ) where

import Barbies.Layered             (IsBarbie (Strip, bcover, bstrip))
import Barbies.Layered.Functor (FunctorB (bmap))
import Barbies.Layered.Foldable (FoldableB (bfoldMap))
import Barbies.Layered.Traversable (TraversableB (btraverse))
import Data.Functor.Identity       (Identity (Identity))
import GHC.Generics                (Generic)
import Barbies.Bi (Flip (Flip, runFlip))
import Prelude (Show, Read, Eq, Ord, Applicative (pure), ($), Traversable (sequenceA), (<$>), Monoid (mempty), Semigroup ((<>)), (.))
import Data.Functor.Transformer (FunctorT (tmap))
import Data.Functor.Barbie.Layered (Functor (fmap))
import qualified Prelude

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
type List :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type
#endif

data List a f
  = Nil
  | Cons (f (a f)) (f (List a f))
  deriving Generic

deriving instance (Show (f (a f)), Show (f (List a f))) => Show (List a f)
deriving instance (Read (f (a f)), Read (f (List a f))) => Read (List a f)
deriving instance (Eq (f (a f)), Eq (f (List a f))) => Eq (List a f)
deriving instance (Ord (f (a f)), Ord (f (List a f))) => Ord (List a f)

instance IsBarbie a => IsBarbie (List a) where
  type Strip (List a) = [Strip a]

  bstrip Nil                               = []
  bstrip (Cons (Identity x) (Identity xs)) = bstrip x : bstrip xs

  bcover []       = Nil
  bcover (x : xs) = Cons (Identity $ bcover x) (Identity $ bcover xs)

instance FunctorB a => FunctorB (List a) where
  bmap _ Nil         = Nil
  bmap f (Cons x xs) = Cons (bmap f <$> f x) (bmap f <$> f xs)

instance FoldableB a => FoldableB (List a) where
  bfoldMap _ Nil         = mempty
  bfoldMap f (Cons x xs) = f (bfoldMap f <$> x) <> f (bfoldMap f <$> xs)

instance TraversableB a => TraversableB (List a) where
  btraverse _ Nil = pure Nil
  btraverse f (Cons x xs) = do
    x' <- f x
    x'' <- sequenceA $ btraverse f <$> x'
    xs' <- f xs
    xs'' <- sequenceA $ btraverse f <$> xs'
    pure $ Cons x'' xs''

instance Functor (Flip List) where
  fmap :: (FunctorB (Flip (Flip List) a), Prelude.Functor f) => (a f -> b f) -> Flip List f a -> Flip List f b
  fmap _ (Flip Nil) = Flip Nil
  fmap f (Flip (Cons x xs)) = Flip $ Cons (f <$> x) (runFlip . fmap f . Flip <$> xs)
