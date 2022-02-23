{-# LANGUAGE FlexibleContexts #-}

module Control.Applicative.Barbie.Layered
  ( Applicative (..)
  , (<*)
  , Alternative (..)
  ) where

import Prelude (id)
import Data.Functor.Barbie.Layered (Functor, (<$))
import Data.Function.Barbie.Layered (Function (Function))
import Barbies.Layered.Functor (FunctorB)
import Barbies.Bi (Flip)
import qualified Prelude
import Data.List.Barbie.Layered (List)

class Functor t => Applicative t where
  pure :: a f -> t f a
  (<*>) :: t f (Function a b) -> t f a -> t f b

(<*) :: (Applicative t, FunctorB (Flip t b), Prelude.Functor f) => t f a -> t f b -> t f a
a <* b = (Function id <$ b) <*> a
infixl 4 <*

class Applicative t => Alternative t where
  empty :: t f a
  (<|>) :: t f a -> t f a -> t f a

some :: Alternative t => t f a -> t (Flip List f) a
some v = Cons <$> v <*> many v

many :: Alternative t => t f a -> t (Flip List f) a
many v = some v <|> pure Nil
