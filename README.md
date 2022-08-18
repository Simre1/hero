# Hero

A implementation of an ECS in Haskell. It tries to be faster than `apecs` by using sparse sets.


## Benchmarks

Benchmarking iteration speed with the following queries:

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
