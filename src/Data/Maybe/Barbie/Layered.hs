{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Data.Maybe.Barbie.Layered
  ( Maybe (..)
  ) where

import           Data.Barbie.Layered   (IsBarbie (Bare, pullOff, putOn))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Functor.Layered  (FoldableB (bfoldMap), FunctorB (bmap), TraversableB (btraverse))
import           GHC.Generics          (Generic)
import           Prelude               (Applicative (pure), Eq, Monoid (mempty), Ord, Read, Show,
                                        Traversable (sequenceA), ($), (<$>))
import qualified Prelude

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
type Maybe :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type
#endif

data Maybe a f
  = Nothing
  | Just (f (a f))
  deriving Generic

deriving instance (Show (f (a f))) => Show (Maybe a f)
deriving instance (Read (f (a f))) => Read (Maybe a f)
deriving instance (Eq (f (a f))) => Eq (Maybe a f)
deriving instance (Ord (f (a f))) => Ord (Maybe a f)

instance IsBarbie a => IsBarbie (Maybe a) where
  type Bare (Maybe a) = Prelude.Maybe (Bare a)

  pullOff Nothing             = Prelude.Nothing
  pullOff (Just (Identity a)) = Prelude.Just $ pullOff a

  putOn Prelude.Nothing  = Nothing
  putOn (Prelude.Just a) = Just $ Identity $ putOn a

instance FunctorB a => FunctorB (Maybe a) where
  bmap _ Nothing  = Nothing
  bmap f (Just a) = Just $ bmap f <$> f a

instance FoldableB a => FoldableB (Maybe a) where
  bfoldMap _ Nothing  = mempty
  bfoldMap f (Just a) = f (bfoldMap f <$> a)

instance TraversableB a => TraversableB (Maybe a) where
  btraverse _ Nothing = pure Nothing
  btraverse f (Just a) = do
    a' <- f a
    a'' <- sequenceA $ btraverse f <$> a'
    pure $ Just a''
