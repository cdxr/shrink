### shrink

`shrink` is a small Haskell library that makes it possible to define
composable shrink functions for
[QuickCheck](http://hackage.haskell.org/package/QuickCheck).


#### the problem

Consider the case of writing an
[Arbitrary](http://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Arbitrary.html)
instance.

```haskell
class Arbitrary a where
    arbitrary :: Gen a
    shrink :: a -> [a]
```

We will use the following type as an example:

```haskell
data T a = T Int Int a
```

We would like to define an `Arbitrary` instance for `T a`. The value
of `arbitrary` is a `Gen (T a)`. This can be defined in a clear and
composable manner because `Gen` is an instance of `Applicative`.

```haskell
instance (Arbitrary a) => Arbitrary (T a) where
    arbitrary (T x y a) = T <$> arbitrary <*> arbitrary <*> arbitrary
```

Using the idiomatic combinators `<$>` and `<*>` we easily map the `T`
constructor over a `Gen Int`, another `Gen Int`, and a `Gen a`.
The result, of type `Gen (T a)`, is composable and can be used to define
`arbitrary` for types that contain a `T a`.

Now lets consider the definition of `shrink`. The function `shrink`
should take a value and provide a list of "shrinks", values that are
(in some subjective sense) smaller than that value. If the
argument to `shrink` cannot be made smaller, the result is an empty list.

We know that a `T a` is made up of an `Int`, another `Int`, and an `a`, all
of which have defined `shrink`. A definition with the desired semantics
follows:

```haskell
    shrink (T x y a) = [ T x' y a | x' <- shrink x ]
                    ++ [ T x y' a | y' <- shrink y ]
                    ++ [ T x y a' | a' <- shrink a ]
```

To shrink a `T a`, you take every shrink of each component and combine it with
every other original component. Note how verbose this definition is.
It would be nice if there was a shorter, simpler, and more composable way to
express the same thing.

At first glance it might seem that you can use the applicative instance
for `[]` to achieve this. Then the definition would be:

```haskell
    -- this is wrong
    shrink (T x y a) = T <$> shrink x <*> shrink y <*> shrink a
```

But this does not have the desired semantics. Every shrink of every
component is combined with every shrink of every other component. Even
worse, if a single component has no more shrinks then the result for the
whole is no shrinks.


#### the solution

The `Test.Shrink` module provides the type `Shrink a`,
representing a value of `a` and its possible shrinks. The desired
functionality is provided by just two functions:

```haskell
-- like `shrink`, but returns `Shrink a` instead of `[a]`.
shrinks :: (Arbitrary a) => a -> Shrink a

runShrink :: Shrink a -> [a]
```

We can write the desired definition as:

```haskell
    shrink (T x y a) = runShrink $ T <$> shrinks x <*> shrinks y <*> shrinks a
```

The `Shrink` instance of `Applicative` provides the desired semantics: each
possible shrink of each component is combined with the original value of every
other component. The list of possible shrinks will only be empty if there are
no possible shrinks for any component.
