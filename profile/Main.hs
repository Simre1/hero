{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import Hex.Internal.Component
import Hex.Internal.Component.UnboxedSet
import Hex.Internal.Entity (Entity (..), MaxEntities (MaxEntities))
import Hex.Internal.System
import Hex.Internal.World

newtype Position = Position (Int, Int) deriving (Unboxable)

instance Component Position where
  componentStorage = unboxedSet

newtype Velocity = Velocity (Int, Int) deriving (Unboxable)

instance Component Velocity where
  componentStorage = unboxedSet

main :: IO ()
main = do
  !world <- makeWorld
  runSystem world testHex2

makeWorld :: IO World
makeWorld = do
  world <- newWorld 10000
  worldAddComponentStorage world $ Proxy @Position
  worldAddComponentStorage world $ Proxy @Velocity

  runSystem world $ do
    forM_ [0 .. 1000] $ \i -> do
      newEntity (Position (i, 0), Velocity (2, 1))
  pure world

testHex2 :: System IO ()
testHex2 = do
  forM_ [0 .. 1500] $ \_ -> do
    cMap $ \(Position (x, y), Velocity (vx, vy)) -> Position (x + vx, y + vy)
