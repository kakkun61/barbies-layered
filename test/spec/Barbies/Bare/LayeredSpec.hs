{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Barbies.Bare.LayeredSpec where

import Barbies.Bare.Layered
import Data.Functor.Barbie.Layered

import Data.Functor.Identity
import Test.Hspec

spec :: Spec
spec = do
  it "bstripFrom" $ do
    bstripFrom runBar (Foo (Bar 1)) `shouldBe` Foo 1

  it "bcoverWith" $ do
    bcoverWith Bar (Foo 1) `shouldBe` Foo (Bar 1)

newtype Foo b f = Foo (Wear b f Int)

deriving instance Show (Wear b f Int) => Show (Foo b f)
deriving instance Eq (Wear b f Int) => Eq (Foo b f)

instance FunctorB (Foo Covered) where
  bmap f (Foo a) = Foo (f a)

instance BareB Foo where
  bstrip (Foo (Identity a)) = Foo a
  bcover (Foo a) = Foo $ Identity a

newtype Bar a = Bar { runBar :: a } deriving (Show, Eq, Functor)
