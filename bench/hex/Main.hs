{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# LANGUAGE UnliftedNewtypes #-}

import Control.Monad
import Control.Monad.IO.Class
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Test.Tasty.Bench
import qualified Data.SparseSet.Storable as SV
import Hex

data Position = Position {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Position

data Velocity = Velocity {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Velocity

data Acceleration = Acceleration {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Acceleration

instance Component Position where
  type Store Position = SparseSetStorableStore

instance Component Velocity where
  type Store Velocity = SparseSetStorableStore

instance Component Acceleration where
  type Store Acceleration = SparseSetStorableStore

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
physics = foldl (*>) sys $ const sys <$> [1..20]
  where sys = 
          cmap (\(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)) *>
          cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))



        -- *> cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))
        -- *> cfoldl (\s (Position x y) -> s + x + y) (0 :: Int)
        -- *> pure ()
