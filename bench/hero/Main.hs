{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

import Control.Monad ( forM_ )
import qualified Hero.SparseSet.Storable as SV
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Hero
    ( Component(Store),
      StorableSparseSet,
      World,
      newWorld,
      System,
      compileSystem,
      cmap,
      newEntity )
import Test.Tasty.Bench ( bench, defaultMain, whnfIO )

data Position = Position {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Position

data Velocity = Velocity {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Velocity

data Acceleration = Acceleration {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Acceleration

instance Component Position where
  type Store Position = StorableSparseSet

instance Component Velocity where
  type Store Velocity = StorableSparseSet

instance Component Acceleration where
  type Store Acceleration = StorableSparseSet

main :: IO ()
main = do
  world <- physicsWorld
  run <- compileSystem physics world
  defaultMain $
    [ bench "simple physics (3 components)" $ whnfIO $ run ()
    ]

physicsWorld :: IO World
physicsWorld = do
  world <- newWorld 10000
  initEntities <- compileSystem newEntity world
  forM_ [0 .. 1000] $ \i ->
    initEntities (Position 0 i, Velocity 0 0, Acceleration 1 0)

  pure world

physics :: System IO () ()
physics = foldl (*>) sys $ const sys <$> [1 .. 20]
  where
    sys =
      cmap (\(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay))
        *> cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))
