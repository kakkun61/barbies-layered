{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Data.List.NonEmpty.Barbie.Layered
  ( List1 (..)
  ) where

import           Data.Barbie.Layered      (IsBarbie (Bare, pullOff, putOn))
import           Data.Functor.Identity    (Identity (Identity))
import           Data.Functor.Layered     (FoldableB (bfoldMap), FunctorB (bmap), TraversableB (btraverse))
import           Data.List.Barbie.Layered (List)
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as Base
import           GHC.Generics             (Generic)

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
type List1 :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type
#endif

data List1 a f =
  (f (a f)) :| (f (List a f))
  deriving Generic

deriving instance (Show (f (a f)), Show (f (List a f))) => Show (List1 a f)
deriving instance (Read (f (a f)), Read (f (List a f))) => Read (List1 a f)
deriving instance (Eq (f (a f)), Eq (f (List a f))) => Eq (List1 a f)
deriving instance (Ord (f (a f)), Ord (f (List a f))) => Ord (List1 a f)

instance IsBarbie a => IsBarbie (List1 a) where
  type Bare (List1 a) = NonEmpty (Bare a)

  pullOff ((Identity x) :| (Identity xs)) = pullOff x Base.:| pullOff xs

  putOn (x Base.:| xs) = Identity (putOn x) :| Identity (putOn xs)

instance FunctorB a => FunctorB (List1 a) where
  bmap f (x :| xs) = (bmap f <$> f x) :| (bmap f <$> f xs)

instance FoldableB a => FoldableB (List1 a) where
  bfoldMap f (x :| xs) = f (bfoldMap f <$> x) <> f (bfoldMap f <$> xs)

instance TraversableB a => TraversableB (List1 a) where
  btraverse f (x :| xs) = do
    x' <- f x
    x'' <- sequenceA $ btraverse f <$> x'
    xs' <- f xs
    xs'' <- sequenceA $ btraverse f <$> xs'
    pure $ x'' :| xs''
