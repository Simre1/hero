{-# LANGUAGE AllowAmbiguousTypes #-}
module Hex.Internal.World where

import Data.Word (Word32)
import Hex.Internal.Component
import Hex.Internal.Entity

data World = World
  { worldStores :: {-# UNPACK #-} !Stores,
    worldEntities :: {-# UNPACK #-} !Entities,
    worldMaxEntities :: {-# UNPACK #-} !MaxEntities
  }

newWorld :: Word32 -> IO World
newWorld !max = do
  let maxEnts = MaxEntities max
  !stores <- newStores
  !entities <- newEntities maxEnts
  pure $ World stores entities maxEnts

worldComponentId :: forall component. Component component => Component component => World -> IO ComponentId
worldComponentId !w = getComponentId @component (worldStores w) (worldMaxEntities w)
{-# INLINE worldComponentId #-}

worldComponent :: forall component. Component component => Component component => World -> IO (Store' component)
worldComponent !w = getComponentId @component (worldStores w) (worldMaxEntities w) >>= getStore (worldStores w)
{-# INLINE worldComponent #-}

worldNewEntity :: World -> IO Entity
worldNewEntity !world = newEntity (worldMaxEntities world) (worldEntities world)
{-# INLINE worldNewEntity #-}
