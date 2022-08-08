{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Main where

import Apecs (Has, Storage, SystemT (..), asks, explInit)
import Apecs qualified as A
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

data Velocity = Velocity Int Int deriving (Generic)

instance GStorable Velocity

instance Component Velocity where
  componentStorage = storedSet

data Acceleration = Acceleration Int Int deriving (Generic)

instance GStorable Acceleration

instance Component Acceleration where
  componentStorage = storedSet


A.makeWorldAndComponents "ApecsWorld" [''Position, ''Velocity, ''Acceleration]

main :: IO ()
main = do
  !world <- makeWorld
  apecsWorld <- makeApecsWorld
  -- runSystem world testHex2
  -- pure ()

  defaultMain
    [ bench "hex" $ whnfIO $ runSystem world testHex2,
      bench "apecs" $ whnfIO $ A.runSystem testApecs2 apecsWorld
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

makeApecsWorld :: IO ApecsWorld
makeApecsWorld = do
  apecsWorld <- initApecsWorld

  flip A.runSystem apecsWorld $ do
    forM_ [0 .. 2000] $ \i -> do
      e <- A.newEntity (Position 0 1, Velocity 2 1, Acceleration 1 0)
      pure ()
  pure apecsWorld

testHex2 :: System IO ()
testHex2 = do
  forM_ [0 .. 1000] $ \_ -> do
    cMap $ \(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)
    cMap $ \(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy)
    cFoldl (\s (Position x y) -> s + x + y) (0 :: Int)

  pure ()

testApecs2 :: A.System ApecsWorld ()
testApecs2 = do
  forM_ [0 .. 1000] $ \_ -> do
    A.cmap $ \(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)
    A.cmap $ \(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy)
    A.cfold (\s (Position x y) -> s + x + y) (0 :: Int)

  pure ()
