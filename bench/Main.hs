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
import Hex.Internal.NewSystem
import Hex.Internal.World
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)
import Data.Data
import Test.Tasty.Bench
import Data.Functor
import Control.Arrow

data Position = Position Int Int deriving (Generic)

instance GStorable Position

data Velocity = Velocity Int Int deriving (Generic)

instance GStorable Velocity

data Acceleration = Acceleration Int Int deriving (Generic)

instance GStorable Acceleration

instance Component Position where
  componentStorage = storedSet

instance Component Velocity where
  componentStorage = storedSet

instance Component Acceleration where
  componentStorage = storedSet

main :: IO ()
main = do
  -- runSystem world testHex2
  -- pure ()
  world <- physicsWorld
  run <- compileSystem physics world
  defaultMain $ 
    [ bench "simple physics (3 components)" $ whnfIO $ run ()
    ]

physicsWorld :: IO World
physicsWorld = do
  world <- newWorld 10000
  worldComponent @Position world
  worldComponent @Velocity world
  worldComponent @Acceleration world


  make <- compileSystem newEntity world
  
  forM_ [0..1000] $ \i ->
    make (Position 0 i, Velocity 0 0, Acceleration 1 0)
  
  pure world

physics :: System IO () ()
physics = foldl (>>>) sys $ const sys <$> [1..500]
  where sys = 
          cmap (\(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)) >>>
          cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))
          >>> cfoldl (\s (Position x y) -> s + x + y) (0 :: Int) >>> pure ()
            -- *> cmapM (\(Position x y) -> print y)
          -- >>> SystemMap (liftIO . print)
            
