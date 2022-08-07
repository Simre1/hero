module Main where

import qualified Data.SparseSet.Unboxed as U
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?))
import Data.Functor ((<&>))


main :: IO ()
main =  defaultMain $ sparseSet

sparseSet :: TestTree
sparseSet = testGroup "SparseSet" [
       testGroup "Unboxed" [
                testCase "Insert/Lookup/Remove" $ do
                    set <- U.create @Int 5 5
                    (0 ==) <$> U.size set @? "Size should be 0"
                    U.insert set 0 10
                    U.insert set 1 11
                    U.insert set 2 12
                    (== Just 10) <$> U.lookup set 0 @? "Lookup 0 is not Just 10"
                    (== Just 11) <$> U.lookup set 1 @? "Lookup 1 is not Just 11"
                    (== Just 12) <$> U.lookup set 2 @? "Lookup 2 is not Just 12"
                    (== Nothing) <$> U.lookup set 3 @? "3 is not in the set"
                    U.contains set 1 @? "1 should be in the set"
                    (== 3) <$> U.size set @? "Size should be 3"
                    U.remove set 1
                    (== 2) <$> U.size set @? "Size should be 2"
                    (== Nothing) <$> U.lookup set 1 @? "1 should be no longer in the set"
                    (== Just 10) <$> U.lookup set 0 @? "Lookup 0 should be Just 10"
                    (== Just 12) <$> U.lookup set 2 @? "Lookup 12 should be Just 12"
                    U.remove set 0
                    U.remove set 2
                    (== 0) <$> U.size set @? "Set should be empty"
                    not <$> U.contains set 0 @? "Set should be empty"
                    not <$> U.contains set 2 @? "Set should be empty"
            ]
    ]
