# Hero SDL2

Hero SDL2 provides components and systems to use SDL2 in combination with Hero. Currently, it can do very simple 2D rendering and keyboard input.

## Prerequisites

Hero SDL2 needs to have up to date versions of the C libraries SDL2, SDL2_gfx, and SDL2_image. Then, it can be added as a build dependency
like other libraries.

## Run examples

Hero SDL2 has a few examples within the `examples` folder.

The `rotating-shapes` examples shows how to render rectangle shapes and how to rotate / move shapes.

```
cabal run rotating-shapes
```

The `image-rendering` example shows simple image loading / rendering. 
```
cabal run image-rendering
```

The `move-shape` example shows how to get keyboard input and lets you move a circle with the arrow keys. 
```
cabal run move-shape
```




