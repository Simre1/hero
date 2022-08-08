{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Apecs (Has, Storage, SystemT (..), asks, explInit)
import Apecs qualified as A
import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import Hex.Internal.Component
import Hex.Internal.Component.UnboxedSet
import Hex.Internal.Entity (Entity (..), MaxEntities (MaxEntities))
import Hex.Internal.System
import Hex.Internal.World
import Test.Tasty.Bench

newtype Position = Position (Int, Int) deriving (Unboxable)

instance Component Position where
  componentStorage = unboxedSet

newtype Velocity = Velocity (Int, Int) deriving (Unboxable)

instance Component Velocity where
  componentStorage = unboxedSet

A.makeWorldAndComponents "ApecsWorld" [''Position, ''Velocity]

main :: IO ()
main = do
  !world <- makeWorld
  -- apecsWorld <- makeApecsWorld
  -- runSystem world testHex2
  -- pure ()

  defaultMain
    [ bench "hex" $ nfIO $ runSystem world testHex2
      -- ,bench "apecs" $ nfIO $ A.runSystem testApecs2 apecsWorld
      
    ]

-- testHex :: IO ()
-- testHex = do
--   world <- newWorld 10000
--   worldAddComponentStorage world $ Proxy @Position
--   worldAddComponentStorage world $ Proxy @Velocity

--   runSystem world $ do
--     forM_ [0 .. 4000] $ \i -> do
--       newEntity (Position (i, 0), Velocity (2, 1))

--     forM_ [0 .. 500] $ \_ -> do
--       cMap $ \(Position (x, y), Velocity (vx, vy)) -> Position (x + vx, y + vy)

-- testApecs :: IO ()
-- testApecs = do
--   apecsWorld <- initApecsWorld

--   flip A.runSystem apecsWorld $ do
--     forM_ [0 .. 4000] $ \i -> do
--       A.newEntity (Position (i, 0), Velocity (2, 1))
--     forM_ [0 .. 500] $ \_ -> do
--       A.cmap $ \(Position (x, y), Velocity (vx, vy)) -> Position (x + vx, y + vy)

makeWorld :: IO World
makeWorld = do
  world <- newWorld 10000
  worldAddComponentStorage world $ Proxy @Position
  worldAddComponentStorage world $ Proxy @Velocity

  runSystem world $ do
    forM_ [0 .. 1000] $ \i -> do
      newEntity (Position (i, 0), Velocity (2, 1))
  pure world

makeApecsWorld :: IO ApecsWorld
makeApecsWorld = do
  apecsWorld <- initApecsWorld

  flip A.runSystem apecsWorld $ do
    forM_ [0 .. 1000] $ \i -> do
      e <- A.newEntity (Position (i, 0), Velocity (2, 1))
      pure ()
  pure apecsWorld

testHex2 :: System IO ()
testHex2 = do
  forM_ [0 .. 1500] $ \_ -> do
    cMap $ \(Position (x, y), Velocity (vx, vy)) -> Position (x + vx, y + vy)

testApecs2 :: A.System ApecsWorld ()
testApecs2 = do
  forM_ [0 .. 1500] $ \_ -> do
    A.cmap $ \(Position (x, y), Velocity (vx, vy)) -> Position (x + vx, y + vy)
