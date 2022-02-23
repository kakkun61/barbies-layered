{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Tuple.Barbie.Bare.Layered
  ( Tuple2 (..)
  ) where

import           Barbies.Bare                (Bare, Covered)
import           Barbies.Layered             (BareB, Implicit (Implicit), IsBarbie (Strip, bcover, bstrip),
                                              StripImplicit (StripImplicit))
import           Barbies.Layered.Functor (FunctorB (bmap))
import Barbies.Layered.Foldable (FoldableB (bfoldMap))
import Barbies.Layered.Traversable (TraversableB (btraverse))
import           Data.Functor.Identity       (Identity (Identity))
import qualified Data.Tuple.Barbie.Layered   as L

data Tuple2 t1 t2 b f where
  BareTuple2 :: (t1 Bare f, t2 Bare f) -> Tuple2 t1 t2 Bare f
  CoveredTuple2 :: L.Tuple2 (Implicit t1) (Implicit t2) f -> Tuple2 t1 t2 Covered f

instance (IsBarbie (t1 Covered), IsBarbie (t2 Covered), Strip (t1 Covered) ~ t1 Bare Identity, Strip (t2 Covered) ~ t2 Bare Identity) => IsBarbie (Tuple2 t1 t2 Covered) where
  type Strip (Tuple2 t1 t2 Covered) = Tuple2 t1 t2 Bare Identity

  bstrip (CoveredTuple2 t) =
    let (StripImplicit v1, StripImplicit v2) = bstrip t
    in BareTuple2 (v1, v2)

  bcover (BareTuple2 (v1, v2)) = CoveredTuple2 $ L.Tuple2 (Identity $ Implicit $ bcover v1) (Identity $ Implicit $ bcover v2)

instance (FunctorB (t1 Covered), FunctorB (t2 Covered), IsBarbie (t1 Covered), IsBarbie (t2 Covered), Strip (t1 Covered) ~ t1 Bare Identity, Strip (t2 Covered) ~ t2 Bare Identity) => BareB (Tuple2 t1 t2)

instance (FunctorB (t1 Covered), FunctorB (t2 Covered)) => FunctorB (Tuple2 t1 t2 Covered) where
  bmap f (CoveredTuple2 t) = CoveredTuple2 $ bmap f t
