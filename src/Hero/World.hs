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
import GHC.Generics ( Generic )
import Hero.Component.Component ( Store', Component, ComponentId )
import Hero.Component.Store.AllStores qualified as AllStores
import Hero.Entity
  ( Entities,
    Entity,
    MaxEntities (..),
    entitiesNew,
    newEntities,
  )
import Optics.Core ((^.))

-- | A world holds all entities and their components.
data World = World
  { allStores :: !AllStores.AllStores,
    entities :: !Entities,
    maxEntities :: !MaxEntities
  }
  deriving (Generic)

getComponentId :: forall (component :: Type). Component component => World -> IO ComponentId
getComponentId !w = AllStores.getComponentId @component (w ^. #allStores) (w ^. #maxEntities)
{-# INLINE getComponentId #-}

getStore :: forall component. Component component => World -> IO (Store' component)
getStore !w = AllStores.getComponentId @component (w ^. #allStores) (w ^. #maxEntities) >>= AllStores.getStore (w ^. #allStores)
{-# INLINE getStore #-}

addStore :: forall component. Component component => World -> Store' component -> IO ComponentId
addStore !w s = AllStores.addStore (w ^. #allStores) s
{-# INLINE addStore #-}

createEntity :: World -> IO Entity
createEntity !world = entitiesNew (world ^. #maxEntities) (world ^. #entities)
{-# INLINE createEntity #-}

-- | Create a new world which can hold as many entities as were specified in the input parameter.
--   Keep in mind that a greater maximum results in greater memory usage.
createWorld :: Word32 -> IO World
createWorld !max = do
  let maxEnts = MaxEntities max
  !allStores <- AllStores.createStores
  !entities <- newEntities maxEnts
  pure $ World allStores entities maxEnts
