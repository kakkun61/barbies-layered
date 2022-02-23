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

module Data.Tuple.Barbie.Layered
  ( Tuple2 (..)
  ) where

import Barbies.Layered             (IsBarbie (Strip, bcover, bstrip))
import           Barbies.Layered.Functor (FunctorB (bmap))
import Barbies.Layered.Foldable (FoldableB (bfoldMap))
import Barbies.Layered.Traversable (TraversableB (btraverse))
import Data.Functor.Identity       (Identity (Identity))
import GHC.Generics                (Generic)

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
type Tuple2 :: ((Type -> Type) -> Type) -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
#endif

data Tuple2 t1 t2 f =
  Tuple2
    { _1 :: f (t1 f)
    , _2 :: f (t2 f)
    }
  deriving Generic

deriving instance (Show (f (t1 f)), Show (f (t2 f))) => Show (Tuple2 t1 t2 f)
deriving instance (Read (f (t1 f)), Read (f (t2 f))) => Read (Tuple2 t1 t2 f)
deriving instance (Eq (f (t1 f)), Eq (f (t2 f))) => Eq (Tuple2 t1 t2 f)
deriving instance (Ord (f (t1 f)), Ord (f (t2 f))) => Ord (Tuple2 t1 t2 f)

instance (IsBarbie t1, IsBarbie t2) => IsBarbie (Tuple2 t1 t2) where
  type Strip (Tuple2 t1 t2) = (Strip t1, Strip t2)

  bstrip (Tuple2 (Identity a1) (Identity a2)) = (bstrip a1, bstrip a2)

  bcover (a1, a2) = Tuple2 (Identity $ bcover a1) (Identity $ bcover a2)

instance (FunctorB t1, FunctorB t2) => FunctorB (Tuple2 t1 t2) where
  bmap f (Tuple2 a1 a2) = Tuple2 (bmap f <$> f a1) (bmap f <$> f a2)

instance (FoldableB t1, FoldableB t2) => FoldableB (Tuple2 t1 t2) where
  bfoldMap f (Tuple2 a1 a2) = f (bfoldMap f <$> a1) <> f (bfoldMap f <$> a2)

instance (TraversableB t1, TraversableB t2) => TraversableB (Tuple2 t1 t2) where
  btraverse f (Tuple2 a1 a2) = do
    a1' <- f a1
    a1'' <- sequenceA $ btraverse f <$> a1'
    a2' <- f a2
    a2'' <- sequenceA $ btraverse f <$> a2'
    pure $ Tuple2 a1'' a2''
