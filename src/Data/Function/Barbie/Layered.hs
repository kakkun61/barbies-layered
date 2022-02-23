{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Function.Barbie.Layered
  ( Function (..)
  , apply
  , ($)
  , (.)
  ) where

import Prelude (Semigroup, Monoid)
import qualified Prelude
import Data.Kind (Type)

type Function :: ((Type -> Type) -> Type) -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype Function a b f =
  Function (a f -> b f)
  deriving newtype (Semigroup, Monoid)

apply :: Function a b f -> a f -> b f
apply (Function f) = f

($) :: Function a b f -> a f -> b f
($) = apply
infixr 0 $

(.) :: Function b c f -> Function a b f -> Function a c f
Function f . Function g = Function (f Prelude.. g)
infixr 9 .
