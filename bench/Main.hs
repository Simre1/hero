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
import Hex.Internal.NewQuery
import Hex.Internal.World
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Data.Data
import Gauge
import Data.Functor

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
  run <- compileSystem world physics <&> \action -> forM_ [0..2000] (\_ -> action ())
  defaultMain $ 
    [ bench "simple physics (3 components)" $ whnfIO $ run
    ]

physicsWorld :: IO World
physicsWorld = do
  world <- newWorld 10000
  worldComponent @Position world

  make <- compileSystem world $ SystemNewEntity
  
  forM_ [0..2000] $ \i ->
    make (Position 0 i)
  
  pure world

physics :: System IO () ()
physics = do
    cmap $ \(Position x y) -> (Position (x+1) (y+1))
    -- cMap $ \(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy)
    -- cFoldl (\s (Position x y) -> s + x + y) (0 :: Int)
