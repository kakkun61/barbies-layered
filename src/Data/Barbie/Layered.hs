{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies   #-}

module Data.Barbie.Layered
  ( IsBarbie (..)
  ) where

import Data.Functor.Identity (Identity)
import Data.Kind             (Constraint, Type)

type IsBarbie :: ((Type -> Type) -> Type) -> Constraint
class IsBarbie b where
  type Bare b = t | t -> b
  pullOff :: b Identity -> Bare b
  putOn :: Bare b -> b Identity
