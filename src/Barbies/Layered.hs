{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#endif

module Barbies.Layered
  ( IsBarbie (..)
  , strip
  , cover
    -- * Bare values
  , Wear
  , Bare
  , Covered
    -- * Covering and stripping
  , BareB (..)
  , Implicit (..)
  , Explicit (..)
  , StripImplicit (..)
  , StripExplicit (..)
  , Reducible (..)
  ) where

import Barbies.Bare                (Bare, Covered, Wear)
import Control.Category.Const2     (Const2 (Const2))
import Data.Copointed              (Copointed (copoint))
import Barbies.Layered.Functor (FunctorB (bmap))
import Data.Functor.Identity       (Identity (Identity, runIdentity))
import Data.Ix                     (Ix)
import Data.Pointed                (Pointed (point))
import GHC.Exts                    (IsList, IsString)
import GHC.Generics                (Generic)

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Constraint, Type)
import Barbies.Bi (Flip (runFlip))
#else
import Data.Kind (Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
type IsBarbie :: ((Type -> Type) -> Type) -> Constraint
#endif

class IsBarbie b where
  type  Strip b = (t :: Type) | t -> b

  bstrip :: b Identity -> Strip b
  default bstrip :: (b ~ bare Covered, BareB bare, Strip b ~ bare Bare Identity) => b Identity -> Strip b
  bstrip = bstripBare

  bcover :: Strip b -> b Identity
  default bcover :: (b ~ bare Covered, BareB bare, Strip b ~ bare Bare Identity) => Strip b -> b Identity
  bcover = bcoverBare

instance IsBarbie (Const2 a Covered) where
  type Strip (Const2 a Covered) = Const2 a Bare Identity
  bstrip (Const2 a) = Const2 a
  bcover (Const2 a) = Const2 a

strip :: (IsBarbie b, FunctorB b, Functor f, Copointed f) => f (b f) -> Strip b
strip = bstrip . bmap (Identity . copoint) .copoint

cover :: (IsBarbie b, FunctorB b, Functor f, Pointed f) => Strip b -> f (b f)
cover = point . bmap (point . runIdentity) . bcover

-- | Class of Barbie-types defined using 'Wear' and can therefore
-- have 'Bare' versions. Must satisfy:
--
-- @
-- 'bcover' . 'bstrip' = 'id'
-- 'bstrip' . 'bcover' = 'id'
-- @
class FunctorB (b Covered) => BareB b where
  bstripBare :: b Covered Identity -> b Bare Identity
  default bstripBare :: (IsBarbie (b Covered), Strip (b Covered) ~ b Bare Identity) => b Covered Identity -> b Bare Identity
  bstripBare = bstrip

  bcoverBare :: b Bare Identity -> b Covered Identity
  default bcoverBare :: (IsBarbie (b Covered), Strip (b Covered) ~ b Bare Identity) => b Bare Identity -> b Covered Identity
  bcoverBare = bcover

instance FunctorB (Const2 a Covered) => BareB (Const2 a)

-- | Fit a type @a@ that has @'Type' -> ('Type' -> 'Type') -> 'Type'@ kind to @('Type' -> 'Type') -> 'Type'@.
-- In other words remove a type parameter 'Covered' from the type @a@.
type Implicit :: (Type -> (k -> Type) -> Type) -> (k -> Type) -> Type
newtype Implicit a f = Implicit { unimplicit :: a Covered f } deriving stock Generic

deriving stock instance Show (a Covered f) => Show (Implicit a f)
deriving stock instance Read (a Covered f) => Read (Implicit a f)
deriving stock instance Eq (a Covered f) => Eq (Implicit a f)
deriving stock instance Ord (a Covered f) => Ord (Implicit a f)
deriving stock instance Bounded (a Covered f) => Bounded (Implicit a f)
deriving newtype instance Enum (a Covered f) => Enum (Implicit a f)
deriving newtype instance Num (a Covered f) => Num (Implicit a f)
deriving newtype instance Real (a Covered f) => Real (Implicit a f)
deriving newtype instance Integral (a Covered f) => Integral (Implicit a f)
deriving newtype instance Fractional (a Covered f) => Fractional (Implicit a f)
deriving newtype instance Floating (a Covered f) => Floating (Implicit a f)
deriving newtype instance RealFrac (a Covered f) => RealFrac (Implicit a f)
deriving newtype instance RealFloat (a Covered f) => RealFloat (Implicit a f)
deriving newtype instance Ix (a Covered f) => Ix (Implicit a f)
deriving newtype instance IsString (a Covered f) => IsString (Implicit a f)
deriving newtype instance IsList (a Covered f) => IsList (Implicit a f)
deriving newtype instance FunctorB (a Covered) => FunctorB (Implicit a)

instance IsBarbie (a Covered) => IsBarbie (Implicit a) where
  type Strip (Implicit a) = StripImplicit (Strip (a Covered))
  bstrip (Implicit a) = StripImplicit (bstrip a)
  bcover (StripImplicit a) = Implicit (bcover a)

newtype StripImplicit a =
  StripImplicit { unstripImplicit :: a }
  deriving stock (Show, Read, Eq, Ord, Bounded, Generic)
  deriving newtype (Enum, Num, Real, Integral, Fractional, Floating, RealFrac, RealFloat, Ix, IsString, IsList)

-- | Fit a type @a@ that has @('Type' -> 'Type') -> 'Type'@ kind to @'Type' -> ('Type' -> 'Type') -> 'Type'@.
-- In other words add an additional type parameter 'Covered' to the type @a@.
type Explicit :: ((k -> Type) -> Type) -> Type -> (k -> Type) -> Type
data family Explicit a b f

newtype instance Explicit a Covered f = Explicit { unexplicit :: a f } deriving stock Generic

deriving stock instance Show (a f) => Show (Explicit a Covered f)
deriving stock instance Read (a f) => Read (Explicit a Covered f)
deriving stock instance Eq (a f) => Eq (Explicit a Covered f)
deriving stock instance Ord (a f) => Ord (Explicit a Covered f)
deriving stock instance Bounded (a f) => Bounded (Explicit a Covered f)
deriving newtype instance Enum (a f) => Enum (Explicit a Covered f)
deriving newtype instance Num (a f) => Num (Explicit a Covered f)
deriving newtype instance Real (a f) => Real (Explicit a Covered f)
deriving newtype instance Integral (a f) => Integral (Explicit a Covered f)
deriving newtype instance Fractional (a f) => Fractional (Explicit a Covered f)
deriving newtype instance Floating (a f) => Floating (Explicit a Covered f)
deriving newtype instance RealFrac (a f) => RealFrac (Explicit a Covered f)
deriving newtype instance RealFloat (a f) => RealFloat (Explicit a Covered f)
deriving newtype instance Ix (a f) => Ix (Explicit a Covered f)
deriving newtype instance IsString (a f) => IsString (Explicit a Covered f)
deriving newtype instance IsList (a f) => IsList (Explicit a Covered f)
deriving newtype instance FunctorB a => FunctorB (Explicit a Covered)

instance IsBarbie a => IsBarbie (Explicit a Covered) where
  type Strip (Explicit a Covered) = StripExplicit (Strip a)
  bstrip (Explicit a) = StripExplicit (bstrip a)
  bcover (StripExplicit a) = Explicit (bcover a)

newtype StripExplicit a =
  StripExplicit { unstripExplicit :: a }
  deriving stock (Show, Read, Eq, Ord, Bounded, Generic)
  deriving newtype (Enum, Num, Real, Integral, Fractional, Floating, RealFrac, RealFloat, Ix, IsString, IsList)

class Reducible a b where
  reduce :: a -> b

instance {-# OVERLAPPABLE #-} Reducible a a where
  reduce = id

instance Reducible (Explicit (Implicit a) Covered b) (a Covered b) where
  reduce = unimplicit . unexplicit

instance Reducible (Implicit (Explicit a) b) (a b) where
  reduce = unexplicit . unimplicit

instance Reducible (Flip (Flip a) b c) (a b c) where
  reduce = runFlip . runFlip

