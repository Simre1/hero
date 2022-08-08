module Hex.Internal.World where

import Data.Data (Proxy)
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

worldComponentStorage :: Component component => World -> IO (Store component)
worldComponentStorage !w = getComponentStorage (worldStores w) (worldMaxEntities w)
{-# INLINE worldComponentStorage #-}

worldAddComponentStorage :: Component component => World -> Proxy component -> IO ()
worldAddComponentStorage !w = addComponentStorage (worldStores w) (worldMaxEntities w)
{-# INLINE worldAddComponentStorage #-}

worldNewEntity :: World -> IO Entity
worldNewEntity !world = newEntity (worldMaxEntities world) (worldEntities world)
{-# INLINE worldNewEntity #-}
