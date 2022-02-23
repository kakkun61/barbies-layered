{-# LANGUAGE GADTs #-}

module Data.Maybe.Barbie.Bare.Layered
  ( Maybe (..)
  ) where

import           Barbies.Bare              (Bare, Covered)
import           Barbies.Layered           (Implicit)
import qualified Data.Maybe.Barbie.Layered as L
import qualified Prelude

data Maybe a b f where
  BareMaybe :: Prelude.Maybe (a Bare f) -> Maybe a Bare f
  CoveredMaybe :: L.Maybe (Implicit a) f -> Maybe a Covered f
