{-# LANGUAGE CPP                      #-}
{-# LANGUAGE TypeFamilyDependencies   #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Data.Barbie.Layered
  ( IsBarbie (..)
  ) where

import Data.Functor.Identity (Identity)

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Constraint, Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
type IsBarbie :: ((Type -> Type) -> Type) -> Constraint
#endif

class IsBarbie b where
  type Bare b = t | t -> b
  pullOff :: b Identity -> Bare b
  putOn :: Bare b -> b Identity
