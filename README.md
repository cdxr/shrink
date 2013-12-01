### shrink

`shrink` is a small Haskell library that makes it possible to define
composable shrink functions for
[QuickCheck](http://hackage.haskell.org/package/QuickCheck).


#### the problem

Consider the case of writing an
[Arbitrary](http://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Arbitrary.html)
instance for a new type.

Suppose the definition is:

```haskell
data T a = T Int Int a
```
We want to define an `Arbitrary` instance for `T`.

```haskell
class Arbitrary a where
    arbitrary :: Gen a
    shrink :: a -> [a]

The definition of arbitrary is straightforward, because we can compose
the existing definitions for `Int` and `a`.

```haskell
instance (Arbitrary a) => Arbitrary (T a) where
    arbitrary (T x y a) = T <$> arbitrary <*> arbitrary <*> arbitrary
```

The above code utilizes the `Applicative` instance of `Gen`.
Using the idiomatic combinators `<$>` and `<*>` we easily map the `T`
constructor over a `Gen Int`, another `Gen Int`, and a `Gen a`.
The result, of type `Gen (T a)`, is composable and can be used to define
`arbitrary` for types that contain a `T a`.

Now lets consider the definition of `shrink`. A definition with the
desired semantics follows:

```haskell
    shrink (T x y a) = [ T x' y a | x' <- shrink x ]
                    ++ [ T x y' a | y' <- shrink y ]
                    ++ [ T x y a' | a' <- shrink a ]
```

Shrinking a `T` results in all the ways that you can shrink one component
of the `T` combined with the original other components. Note how verbose
this definition is. It would be nice if there was a simple, brief, and
composable way to express the same thing.

At first glance it might seem that you can use the applicative instance
for lists to achieve this. Then the definition would be:

```haskell
    shrink (T x y a) = T <$> shrink x <*> shrink y <*> shrink a
```

But this does not have the desired semantics. Every shrink of every
component is combined with every shrink of every other component. Even
worse, if a single component has no more shrinks then the result for the
whole is no shrinks.


#### the solution

The `Test.QuickCheck.Shrink` module provides the type `Shrink a`,
representing a value of `a` and its possible shrinks. The desired
functionality is provided by just two functions:

```haskell
shrinks :: (Arbitrary a) => Shrink a

runShrink :: Shrink a -> [a]
```

We can write our definition as

```haskell
    shrink (T x y a) = runShrink $ T <$> shrinks x <*> shrinks y <*> shrinks a
```

The `Shrink` instance of `Applicative` provides the desired semantics: each
possible shrink of each component is combined with the original value of every
other component. The list of possible shrinks will only be empty if there are
no possible shrinks for any component.
