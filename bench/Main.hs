{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Hex.Internal.Component
import Hex.Internal.Component.ComponentId
import Hex.Internal.Component.SparseSet
import Hex.Internal.Entity (Entity (..), MaxEntities (MaxEntities))
import Hex.Internal.System
import Hex.Internal.World
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Data.Data
import Gauge

data Position = Position Int Int deriving (Generic)

instance GStorable Position

data Velocity = Velocity Int Int deriving (Generic)

instance GStorable Velocity

data Acceleration = Acceleration Int Int deriving (Generic)

instance GStorable Acceleration

instance Component Position where
  componentStorage = storedSet
  componentId = 0

instance Component Velocity where
  componentStorage = storedSet
  componentId = 1

instance Component Acceleration where
  componentStorage = storedSet
  componentId = 2

instance ComponentAmount where
  componentAmount = 3

main :: IO ()
main = do
  -- runSystem world testHex2
  -- pure ()
  world <- physicsWorld
  defaultMain $ 
    [ bench "simple physics (3 components)" $ whnfIO $ runSystem world physics 
    ]

physicsWorld :: IO World
physicsWorld = do
  world <- newWorld 10000
  worldAddComponentStorage @Position world 
  worldAddComponentStorage @Velocity world 
  worldAddComponentStorage @Acceleration world 

  runSystem world $ do
    forM_ [0 .. 1000] $ \i -> do
      newEntity (Position 0 i, Velocity 0 0, Acceleration 1 0)
  pure world

physics :: System IO ()
physics = do
  forM_ [0 .. 500] $ \_ -> do
    cMap $ \(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)
    cMap $ \(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy)
    cFoldl (\s (Position x y) -> s + x + y) (0 :: Int)

  pure ()
