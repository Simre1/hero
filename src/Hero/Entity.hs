module Hero.Entity (Entity(..), MaxEntities(..), global, Entities, newEntities, entitiesNew, entitiesDelete, entitiesFor, entitiesAmount) where

import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Hero.SparseSet.NoComponent qualified as S
import Data.Vector.Unboxed.Mutable qualified as VU
import Data.Word (Word32)
import Control.Monad.IO.Class (MonadIO)

-- An entity is an object within a world with components. 'Entity' can be used to access the components of a world.
-- Working the the underlying 'World32' is probably not a good idea.
newtype Entity = Entity Word32

-- | MaxEntities should be the maximum amount of live entities within a world. Lower max entity count
-- saves memory.
newtype MaxEntities = MaxEntities Word32

-- | The data structure storing all entities.
data Entities = Entities !S.SparseSetNoComponent !(IORef Word32)

newEntities :: MaxEntities -> IO Entities
newEntities (MaxEntities max) = do
  set <- S.create max max
  lastId <- newIORef 0
  pure $ Entities set lastId

entitiesNew :: MaxEntities -> Entities -> IO Entity
entitiesNew (MaxEntities max) (Entities entities lastIdRef) = do
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
{-# INLINE entitiesNew #-}

entitiesDelete :: Entities -> Entity -> IO ()
entitiesDelete (Entities entities _) (Entity key) = S.remove entities key
{-# INLINE entitiesDelete #-}

entitiesFor :: MonadIO m => Entities -> (Entity -> m ()) -> m ()
entitiesFor (Entities entities _) f = S.for entities (coerce f)
{-# INLINE entitiesFor #-}

entitiesAmount :: Entities -> IO Int
entitiesAmount (Entities entities _) = S.size entities
{-# INLINE entitiesAmount #-}

-- | 'global' is an entity which is not part of normal component store but can
-- be used to access global components like the 'Global' store.
global :: Entity
global = Entity maxBound