# Hero

Hero is an entity component system in Haskell and is inspired by [_apecs_](https://github.com/jonascarpay/apecs). Hero aims to be more performant than _apecs_ by using sparse sets as the main data structure for storing component data. In the future, it also intends to parallelize systems automatically as well.

## Entities

Entities are objects which have components. You can create entities with the `createEntity` system and add components to them.

## Components

Components are Haskell datastructure and mostly contain raw data.

```haskell
data Position = Position Float Float
```

In order to use `Position` as a component, you need to make it an instance of `Component`.

```haskell
instance Component Position where
  type Store Position = BoxedSparseSet 
  -- StorableSparseSet is faster but Position would need to implement Storable
```

## Stores

Each component has a store which holds the component data. Depending on the use of the component, different stores might be best. 
Most important stores are:
- `BoxedSparseSet`: Can be used with any datatype. Each entity has its own component value.
- `StorableSparseSet`: Can be used with `Storable` datatypes. Each entity has its own component value. Faster than `BoxedSparseSet`.
- `Global`: Can be used with any datatype. Each entity has the same component value. Can also be accessed without an entity.

## Systems

Systems are functions which operate on the components of a world. For example, you can map the `Position` component of every entity:

```haskell
cmap_ $ \(Position x y) -> Position (x + 1) y
```

`System`s have the type `system :: System m input output`. `System`s can be chained together through their `Applicative`, `Category` and `Arrow` instance. However, `System`s dot have an instance for `Monad`! If one is familiar with arrowized FRP, then they will feel that `System`s have a similar interface as signal functions.

`System`s do not have a `Monad` instance since they are separated into a compilation and a run phase. Before you can run a `System`, you need to compile it with `compileSystem :: System m input output -> World -> IO (input -> m output)`. In the compilation phase, `System`s look up the used components so that run time is faster.

## Example

```haskell
import Hero
import Data.Foldable (for_)

data Position = Position Int Int

data Velocity = Velocity Int Int

instance Component Position where
  type Store Position = BoxedSparseSet

instance Component Velocity where
  type Store Velocity = BoxedSparseSet

main :: IO ()
main = do
  -- Create the world with a maximum of 10000 live entities
  world <- createWorld 10000

  -- Compile the system
  runSystem <- compileSystem system world

  -- Run the system
  runSystem ()

system :: System IO () ()
system =
  -- Create two entities
  (pure (Position 0 0, Velocity 1 0) >>> createEntity) *>
  (pure (Position 10 0, Velocity 0 1) >>> createEntity) *>

  -- Map position 10 times
  for_ [1..10] (\_ -> cmap_ (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))) *>

  -- Print the current position
  cmapM_ (\(Position x y) -> print (x,y))
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

To generate **documentation**, do:
```
cabal haddock
cabal haddock hero-sdl2
```

### Hero SDL2

The folder hero-sdl2 contains a small libary to use SDL2 with Hero. It needs the C libraries `SDL2`, `SDL2_gdx` and `SDL2_image`.

The Hero SDL2 examples can be run with:

```
cabal run rotating-shapes
cabal run image-rendering
cabal run move-shape
```

More information can be found in [`hero-sdl2`](hero-sdl2/README.md).

### Cabal dependency

To use _Hero_ as a dependency, add `hero` to the `build-depends` section. Additional, create a `cabal.project` with: 
```
packages: *.cabal

source-repository-package
   type: git
   location: https://github.com/Simre1/hero
```

If you also want to use `hero-sdl2`, add `hero-sdl2` to the `build-depends` section as well. The `cabal.project` is then:
```
packages: *.cabal

source-repository-package
   type: git
   location: https://github.com/Simre1/hero

source-repository-package
   type: git
   location: https://github.com/Simre1/hero
   subdir: hero-sdl2
```

## Benchmark

A basic benchmark seems to suggest that `Hero` is much faster. However, it relies heavily on
GHC inlining. It is best to use concrete types when working with systems or add an `INLINE` pragma to 
functions which deal with polymorphic systems.

The following queries are used to test the iteration speed of both libraries:
```
cmap_ (\(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)) *>
cmap_ (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))
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
