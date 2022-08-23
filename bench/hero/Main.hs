{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

import Data.Foldable (for_)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Hero
import Test.Tasty.Bench (bench, defaultMain, whnfIO)

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
  world <- createWorld 10000
  init <- compileSystem initEntities world
  run <- compileSystem physics world

  init ()

  defaultMain $
    [ bench "simple physics (3 components)" $ whnfIO $ run ()
    ]

initEntities :: System IO () ()
initEntities = for_
      [0 .. 1000]
      ( \i -> pure (Position 0 i, Velocity 0 0, Acceleration 1 0) >>> createEntity
      )

physics :: System IO () ()
physics = do
  for_ [1 .. 20] $ \_ -> do
    cmap_ $ \(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)
    cmap_ $ \(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy)
    pure ()

  pure ()
