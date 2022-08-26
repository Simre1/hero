{-# LANGUAGE AllowAmbiguousTypes #-}

module Hero.World
  ( World (..),
    createWorld,
    getComponentId,
    getStore,
    createEntity,
    addStore,
  )
where

import Data.Kind (Type)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Hero.Component.Component (Component, ComponentId, Store')
import Hero.Component.Store.AllStores qualified as AllStores
import Hero.Entity
  ( Entities,
    Entity,
    MaxEntities (..),
    entitiesNew,
    newEntities,
  )
import Hero.Parallel.ExecutionPlanner

import Optics.Core ((^.))

-- | A world holds all entities and their components.
data World = World
  { allStores :: !AllStores.AllStores,
    entities :: !Entities,
    maxEntities :: !MaxEntities
  }
  deriving (Generic)

-- | Get the `ComponentId` for a component. Try to get `ComponentId`s in the compilation phase of systems since component lookup is not very fast.
-- This function will fail if the store for the component
-- was not set up previously and if the store cannot be automatically created.
getComponentId :: forall (component :: Type). Component component => World -> IO ComponentId
getComponentId !w = AllStores.getComponentId @component (w ^. #allStores) (w ^. #maxEntities)
{-# INLINE getComponentId #-}

-- | Gets the store for a component. Try to get `Store'`s in the compilation phase of systems since component lookup is not very fast. This function will fail if the store for the component
-- was not set up previously and if the store cannot be automatically created.
getStore :: forall component. Component component => World -> IO (Store' component)
getStore !w = AllStores.getComponentId @component (w ^. #allStores) (w ^. #maxEntities) >>= AllStores.getStore (w ^. #allStores)
{-# INLINE getStore #-}

-- | Adds a store to the world. It will throw an error if a store for the component already exists.
addStore :: forall component. Component component => World -> Store' component -> IO ComponentId
addStore !w s = AllStores.addStore (w ^. #allStores) s
{-# INLINE addStore #-}

-- | Creates a new entity within the world.
createEntity :: World -> IO Entity
createEntity !world = entitiesNew (world ^. #maxEntities) (world ^. #entities)
{-# INLINE createEntity #-}

-- | Create a new world which can hold as many entities as were specified in the input parameter.
--   Keep in mind that a greater maximum results in greater memory usage. However, the maximum should
-- still be a bit greater than the actual maximum entity amount since creating new entities takes more time
-- for an almost full world.
createWorld :: Word32 -> IO World
createWorld !max = do
  let maxEnts = MaxEntities max
  !allStores <- AllStores.createStores
  !entities <- newEntities maxEnts
  pure $ World allStores entities maxEnts
