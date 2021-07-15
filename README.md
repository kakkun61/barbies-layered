# barbies-layered

[![Hackage](https://matrix.hackage.haskell.org/api/v2/packages/barbies-layered/badge)](http://hackage.haskell.org/package/barbies-layered)

![CI](https://github.com/kakkun61/barbies-layered/workflows/main/badge.svg)

[![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-red?logo=GitHub)](https://github.com/sponsors/kakkun61)

This is like [barbies](https://hackage.haskell.org/package/barbies) but these clothes are layered.

For example when there is a following data type,

```haskell
data Foo = Foo { foo :: [Int] }
```

barbies requires a following.

```haskell
data Foo f = Foo { foo :: f [Int] }
```

But in case of barbies-layered,

```haskell
data Foo f = Foo { foo :: f [f Int] }
```

A typical difference is a type of `bmap`.

```haskell
-- barbies
type FunctorB :: ((k -> Type) -> Type) -> Constraint
class FunctorB b where
  bmap :: (forall a. f a -> g a) -> b f -> b g

-- barbies-layered
type FunctorB :: ((Type -> Type) -> Type) -> Constraint
class FunctorB b where
  bmap :: (Functor f, Functor g) => (forall a. f a -> g a) -> b f -> b g
```
