module Main where

import qualified Data.SparseSet.Storable as S
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?))
import Data.Functor ((<&>))


main :: IO ()
main =  defaultMain $ sparseSet

sparseSet :: TestTree
sparseSet = testGroup "SparseSet" [
       testGroup "Unboxed" [
                testCase "Insert/Lookup/Remove" $ do
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
