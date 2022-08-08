{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Hex.Internal.Component
import Hex.Internal.Component.SparseSet
import Hex.Internal.Entity (Entity (..), MaxEntities (MaxEntities))
import Hex.Internal.System
import Hex.Internal.World
import Test.Tasty.Bench
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

data Position = Position Int Int deriving (Generic)

instance GStorable Position

instance Component Position where
  componentStorage = storedSet
  componentId = 0


data Velocity = Velocity Int Int deriving (Generic)

instance GStorable Velocity

instance Component Velocity where
  componentStorage = storedSet
  componentId = 1


data Acceleration = Acceleration Int Int deriving (Generic)

instance GStorable Acceleration

instance Component Acceleration where
  componentStorage = storedSet
  componentId = 2

instance MaxId where
  maxId = 3

main :: IO ()
main = do
  !world <- makeWorld
  -- runSystem world testHex2
  -- pure ()

  defaultMain
    [ bench "hex" $ whnfIO $ runSystem world testHex2
    ]

makeWorld :: IO World
makeWorld = do
  world <- newWorld 10000
  worldAddComponentStorage @Position world 
  worldAddComponentStorage @Velocity world 
  worldAddComponentStorage @Acceleration world 

  runSystem world $ do
    forM_ [0 .. 2000] $ \i -> do
      newEntity (Position 0 i, Velocity 0 0, Acceleration 1 0)
  pure world

testHex2 :: System IO ()
testHex2 = do
  forM_ [0 .. 1000] $ \_ -> do
    cMap $ \(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)
    cMap $ \(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy)
    cFoldl (\s (Position x y) -> s + x + y) (0 :: Int)

  pure ()
