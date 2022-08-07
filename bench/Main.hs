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

newtype Speed = Speed (Int, Int) deriving (Unboxable)

instance Component Speed where
  componentStorage = unboxedSet

A.makeWorldAndComponents "ApecsWorld" [''Position, ''Speed]

main :: IO ()
main = do
  defaultMain
    [ bench "apecs" $ nfIO $ testApecs,
      bench "hex" $ nfIO $ testHex
    ]

testHex :: IO ()
testHex = do
  world <- newWorld 10000
  worldAddComponentStorage world $ Proxy @Position
  worldAddComponentStorage world $ Proxy @Speed

  runSystem world $ do
    forM_ [0 .. 0] $ \i -> do
      e <- newEntity
      putEntity e (Position (i, 0))

    forM_ [0 .. 1000] $ \_ -> do
      cmap $ \(Position (i, x)) -> Position (i, succ x)

testApecs :: IO ()
testApecs = do

  apecsWorld <- initApecsWorld

  flip A.runSystem apecsWorld $ do
    forM_ [0 .. 2000] $ \i -> do
      e <- A.newEntity (Position (i, 0))
      pure ()    
    forM_ [0 .. 1000] $ \_ -> do
      A.cmap $ \(Position (i, x)) -> Position (i, succ x)
