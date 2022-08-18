# Hero

Hero is an entity component system in Haskell and is inspired by [_apecs_](https://github.com/jonascarpay/apecs). Hero aims to be more performant than _apecs_ by using sparse sets as the main data structure for storing component data.

## World

The `World` stores all entities and the component data. Components can be added to the world during runtime.

## Components

Components are Haskell datastructure and mostly contain raw data.

```haskell
data Position = Position Float Float
```

In order to use `Position` as a component, you need to make it an instance of `Component`.

```haskell
instance Component Position where
  type Store Position = BoxedSparseSet -- StorableSparseSet is faster but Position would need to implement Storable
```

## Systems

Systems are functions which operate on the components of a world. For example, you can map the `Position` component of every entity:

```haskell
cmap $ \(Position x y) -> Position (x + 1) y
```

`System` has the type `system :: System m input output`. `System`s can be chained together through its `Applicative`, `Category` and `Arrow` instance. However, `System`s are not an instance of `Monad`!

After constructing a `System`, you need to compile it with `compileSystem :: System m input output -> World -> IO (input -> m output)`.

## Example

```haskell
import Control.Monad ( forM_ )
import Hero
import Data.Foldable (for_)

data Position = Position Int Int

data Velocity = Velocity Int Int

data Acceleration = Acceleration Int Int

instance Component Position where
  type Store Position = BoxedSparseSet

instance Component Velocity where
  type Store Velocity = BoxedSparseSet


main :: IO ()
main = do
  -- Create the world with a maximum of 10000 live entities
  world <- newWorld 10000

  -- Compile the system
  runSystem <- compileSystem system world

  -- Run the system
  runSystem ()

system :: System IO () ()
system =
  -- Create two entities
  (pure (Position 0 0, Velocity 1 0) >>> newEntity) *>
  (pure (Position 10 0, Velocity 0 1) >>> newEntity) *>

  -- Map position 10 times
  for_ [1..10] (\_ -> cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))) *>

  -- Print the current position
  cmapM (\(Position x y) -> print (x,y))
```

## Installation

To download the project and execute it, you need at least _GHC 9_. I have tested it with _GHC 9.2.1_.

Then, you can run the following commands to build the project.
```
git clone https://github.com/Simre1/hero
cd hero
cabal build all
```

To run the example, do:
```
cabal run example
```

To run the tests, do:
```
cabal test
```

To run the benchmarks, do:
```
cabal run hero-bench
cabal run apecs-bench
```

### Cabal dependency

To use _Hero_ as a dependency, add `hero` to the `build-depends` section. Additional, create a `cabal.project` with: 
```
packages: *.cabal

source-repository-package
   type: git
   location: https://github.com/Simre1/hero
```

## Benchmark

A basic benchmark seems to suggest that `Hero` is much faster. However, it relies heavily on
GHC inlining. It is best to use concrete types when working with systems or add an `INLINE` pragma to 
functions which deal with polymorphic systems.

The following queries are used to test the iteration speed of both libraries:
```
cmap (\(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)) *>
cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))
```

### Hero

```
  simple physics (3 components): OK (0.20s)
    394  μs ±  25 μs
```

### Apecs

```
  simple physics (3 components): OK (0.13s)
    17.5 ms ± 1.5 ms
```
