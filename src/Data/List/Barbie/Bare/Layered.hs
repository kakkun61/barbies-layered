{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Data.List.Barbie.Bare.Layered
  ( List (..)
  ) where

import           Barbies.Bare                (Bare, Covered)
import           Barbies.Layered             (BareB, Implicit, IsBarbie (Strip, bcover, bstrip),
                                              StripImplicit (StripImplicit, unstripImplicit))
import           Barbies.Layered.Functor (FunctorB (bmap))
import           Data.Functor.Identity       (Identity)
import qualified Data.List.Barbie.Layered    as L

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
type List :: (Type -> (Type -> Type) -> Type) -> Type -> (Type -> Type) -> Type
#endif

data List a b f where
  BareList :: [a Bare f] -> List a Bare f
  CoveredList :: L.List (Implicit a) f -> List a Covered f

instance FunctorB (a Covered) => FunctorB (List a Covered) where
  bmap f (CoveredList l) = CoveredList $ bmap f l

instance (IsBarbie (a Covered), Strip (a Covered) ~ a Bare Identity) => IsBarbie (List a Covered) where
  type Strip (List a Covered) = List a Bare Identity
  bstrip (CoveredList l) = BareList $ unstripImplicit <$> bstrip l
  bcover (BareList l) = CoveredList $ bcover $ StripImplicit <$> l

instance (BareB a, IsBarbie (a Covered), Strip (a Covered) ~ a Bare Identity) => BareB (List a)
