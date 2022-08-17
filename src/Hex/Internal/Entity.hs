module Hex.Internal.Entity where

import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.SparseSet.NoComponent qualified as S
import Data.Vector.Unboxed.Mutable qualified as VU
import Data.Word (Word32)

newtype Entity = Entity Word32

newtype MaxEntities = MaxEntities Word32

data Entities = Entities !S.SparseSetNoComponent !(IORef Word32)

newEntities :: MaxEntities -> IO Entities
newEntities (MaxEntities max) = do
  set <- S.create max (max `quot` 3)
  lastId <- newIORef 0
  pure $ Entities set lastId

newEntity :: MaxEntities -> Entities -> IO Entity
newEntity (MaxEntities max) (Entities entities lastIdRef) = do
  lastId <- readIORef lastIdRef
  nextId <- findNext lastId 1
  writeIORef lastIdRef nextId
  S.insert entities nextId
  pure $ Entity nextId
  where
    findNext last distance = do
      let next = last + distance `mod` max
      contained <- S.contains entities next
      if contained
        then findNext last ((distance + 1) ^ 2)
        else pure next
{-# INLINE newEntity #-}

removeEntity :: Entities -> Entity -> IO ()
removeEntity (Entities entities _) (Entity key) = S.remove entities key
{-# INLINE removeEntity #-}

forEntities :: Entities -> (Entity -> IO ()) -> IO ()
forEntities (Entities entities _) f = S.for entities (coerce f)
{-# INLINE forEntities #-}

entityAmount :: Entities -> IO Int
entityAmount (Entities entities _) = S.size entities
{-# INLINE entityAmount #-}
