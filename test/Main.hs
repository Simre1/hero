{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

import Data.Foldable (for_)
import Data.Functor ((<&>))
import qualified Data.SparseSet.Storable as S
import Data.Traversable (for)
import Foreign.Storable.Generic (GStorable)
import Hex.Internal.System
import Hex.Internal.World
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?))
import Data.Monoid
import GHC.Generics
import Control.Arrow
import Hex.Internal.Component
import Hex.Internal.Component.SparseSet

data Position = Position {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Position

data Velocity = Velocity {-# UNPACK #-} !Int  deriving (Generic)



instance Component Position where
  type Store Position = SparseSetStorableStore

instance Component Velocity where
  type Store Velocity = SparseSetStorableStore

instance GStorable Velocity

main :: IO ()
main = defaultMain $ testGroup "Tests" [ecs, sparseSet]

ecs :: TestTree
ecs =
  testGroup
    "ECS"
    [ testCase "Iteration" $ do
        w <- newWorld 10000
        init <- compileSystem (for_ [0 .. 999] (\_ -> pure (Position 0, Velocity 1) >>> newEntity)) w
        init ()
        let system =
              cmap (\(Position x, Velocity v) -> Position (x + v)) *>
              cmap (\(Velocity v) -> Velocity (v + 1)) *>
              cfold (\(Position x) -> Sum x)
        runSystem <- compileSystem system w
        result <- runSystem ()
        (== Sum 1000) result @? "Result is false" 
        result <- runSystem ()
        (== Sum 3000) result @? "Result is false" 
    ]

sparseSet :: TestTree
sparseSet =
  testGroup
    "SparseSet"
    [ testGroup
        "Storable"
        [ testCase "Insert/Lookup/Remove" $ do
            set <- S.create @Int 5 5
            (0 ==) <$> S.size set @? "Size should be 0"
            S.insert set 0 10
            S.insert set 1 11
            S.insert set 2 12
            (== Just 10) <$> S.lookup set 0 @? "Lookup 0 is not Just 10"
            (== Just 11) <$> S.lookup set 1 @? "Lookup 1 is not Just 11"
            (== Just 12) <$> S.lookup set 2 @? "Lookup 2 is not Just 12"
            (== Nothing) <$> S.lookup set 3 @? "3 is not in the set"
            S.contains set 1 @? "1 should be in the set"
            (== 3) <$> S.size set @? "Size should be 3"
            S.remove set 1
            (== 2) <$> S.size set @? "Size should be 2"
            (== Nothing) <$> S.lookup set 1 @? "1 should be no longer in the set"
            (== Just 10) <$> S.lookup set 0 @? "Lookup 0 should be Just 10"
            (== Just 12) <$> S.lookup set 2 @? "Lookup 12 should be Just 12"
            S.remove set 0
            S.remove set 2
            (== 0) <$> S.size set @? "Set should be empty"
            not <$> S.contains set 0 @? "Set should be empty"
            not <$> S.contains set 2 @? "Set should be empty"
        ]
    ]
