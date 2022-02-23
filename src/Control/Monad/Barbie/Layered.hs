module Control.Monad.Barbie.Layered
 ( Monad (..)
 , (=<<)
 ) where

import Prelude (flip)
import Control.Applicative.Barbie.Layered (Applicative)

class Applicative f => Monad f where
  (>>=) :: f g a -> (a g -> f g b) -> f g b
  infixl 1 >>=

(=<<) :: Monad f => (a g -> f g b) -> f g a -> f g b
(=<<) = flip (>>=)
infixr 1 =<<
