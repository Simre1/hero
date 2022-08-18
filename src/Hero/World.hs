{-# LANGUAGE AllowAmbiguousTypes #-}

module Hero.World (
  World(..),
  newWorld,
  worldComponentId,
  worldComponent,
  worldNewEntity
) where

import Data.Word (Word32)
import Hero.Component
  ( Component,
    ComponentId,
    Store',
    Store,
    Stores,
    getComponentId,
    getStore,
    newStores,
    ComponentMakeStore
  )
import Hero.Entity
  ( Entities,
    Entity,
    MaxEntities (..),
    newEntities,
    newEntity,
  )
import Data.Kind (Type)

data World = World
  { worldStores :: !Stores,
    worldEntities :: !Entities,
    worldMaxEntities :: !MaxEntities
  }

-- | Create a new world which can hold as many entities as were specified in the input parameter.
--   Keep in mind that a greater maximum results in greater memory usage.
newWorld :: Word32 -> IO World
newWorld !max = do
  let maxEnts = MaxEntities max
  !stores <- newStores
  !entities <- newEntities maxEnts
  pure $ World stores entities maxEnts

worldComponentId :: forall (component :: Type). Component component => World -> IO ComponentId
worldComponentId !w = getComponentId @component (worldStores w) (worldMaxEntities w)
{-# INLINE worldComponentId #-}

worldComponent :: forall component. Component component => World -> IO (Store' component)
worldComponent !w = getComponentId @component (worldStores w) (worldMaxEntities w) >>= getStore (worldStores w)
{-# INLINE worldComponent #-}

worldNewEntity :: World -> IO Entity
worldNewEntity !world = newEntity (worldMaxEntities world) (worldEntities world)
{-# INLINE worldNewEntity #-}