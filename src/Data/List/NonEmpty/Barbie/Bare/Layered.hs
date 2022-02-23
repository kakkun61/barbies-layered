{-# LANGUAGE GADTs #-}

module Data.List.NonEmpty.Barbie.Bare.Layered
  ( List1 (..)
  ) where

import           Barbies.Bare                      (Bare, Covered)
import           Barbies.Layered                   (Implicit)
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.List.NonEmpty.Barbie.Layered as L

data List1 a b f where
  BareList1 :: NonEmpty (a Bare f) -> List1 a Bare f
  CoveredList1 :: L.List1 (Implicit a) f -> List1 a Covered f
