{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Hex.Component
import Hex.Component.SparseSet
import Hex.Entity (Entity (..), MaxEntities (MaxEntities))
import Hex.System
import Hex.World
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Control.Arrow

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
  run ()

physicsWorld :: IO World
physicsWorld = do
  world <- newWorld 10000

  make <- compileSystem newEntity world
  
  forM_ [0..1000] $ \i ->
    make (Position 0 i, Velocity 0 0, Acceleration 1 0)
  
  pure world

physics :: System IO () ()
physics = foldl (*>) sys $ const sys <$> [1..300]
  where sys = 
          cmap (\(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)) *>
          cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))
          *> cfoldl (\s (Position x y) -> s + x + y) (0 :: Int) *> pure ()
