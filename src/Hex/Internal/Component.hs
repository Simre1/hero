{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Hex.Internal.Component where

-- import Data.HashTable.IO as H

import Control.Monad.IO.Class
import Data.IORef
-- import Data.Map.Strict qualified as M

import Data.Map qualified as M
import Data.Proxy
import Data.Vector.Mutable qualified as V
import Hex.Internal.Component.ComponentId
import Hex.Internal.Entity
import Type.Reflection
import Unsafe.Coerce
import Data.SparseSet.Storable
import qualified Data.SparseSet.Storable as SV
import Data.Coerce
import Data.Vector.Storable (Storable)


newtype ComponentId = ComponentId {unwrapComponentId :: Int} deriving (Show)

class (Storable component, Typeable component) => Component component where
  componentStorage :: MaxEntities -> IO (Store component)

newtype WrappedStorage = WrappedStorage (forall component. Store component)

unwrapWrappedStorage :: WrappedStorage -> Store component
unwrapWrappedStorage (WrappedStorage s) = s

class StoreClass store component where
  storeClassContains :: store component -> Entity -> IO Bool
  storeClassGet :: store component -> Entity -> IO component
  storeClassPut :: store component -> Entity -> component -> IO ()
  storeClassDelete :: store component -> Entity -> IO ()
  storeClassFor :: store component -> (Entity -> component -> IO ()) -> IO ()
  storeClassMembers :: store component -> IO Int

-- data Store component where
--   Store :: StoreClass store component => store component -> Store component

newtype Store component = Store (SparseSetStorable component)


data Stores = Stores (IORef (V.IOVector WrappedStorage)) (IORef (M.Map SomeTypeRep ComponentId))

addStore :: forall component. Component component => Stores -> Store component -> IO ComponentId
addStore (Stores storeVecRef mappingsRef) store = do
  mappings <- readIORef mappingsRef
  storeVec <- readIORef storeVecRef
  let rep = someTypeRep $ Proxy @component
      maybeMapping = M.lookup rep mappings
      wrappedStore = (WrappedStorage $ unsafeCoerce store)
  case maybeMapping of
    Nothing -> do
      let newId = M.size mappings
          storeSize = V.length storeVec
      if newId < storeSize
        then V.unsafeWrite storeVec newId wrappedStore
        else do
          newStoreVec <- V.grow storeVec (storeSize `quot` 2)
          V.unsafeWrite newStoreVec newId wrappedStore
          writeIORef storeVecRef newStoreVec
      modifyIORef mappingsRef $ M.insert rep (ComponentId newId)
      pure $ ComponentId newId
    Just i -> V.unsafeWrite storeVec (unwrapComponentId i) wrappedStore *> pure i

addComponentStore :: forall component. (Component component) => Stores -> MaxEntities -> IO ComponentId
addComponentStore stores@(Stores storeVec mappings) max = do
  store <- componentStorage @component max
  addStore stores store

getStore :: Stores -> ComponentId -> IO (Store component)
getStore (Stores storeVecRef _) componentId =
  readIORef storeVecRef >>= \vec -> unwrapWrappedStorage <$> V.unsafeRead vec (unwrapComponentId componentId)

getComponentId :: forall component. Component component => Stores -> MaxEntities -> IO ComponentId
getComponentId stores@(Stores _ mappingsRef) max = do
  maybeComponent <- M.lookup (someTypeRep $ Proxy @component) <$> readIORef mappingsRef
  case maybeComponent of
    Just componentId -> pure componentId
    Nothing -> do
      store <- componentStorage @component max
      componentId <- addStore stores store
      pure componentId

newStores :: IO Stores
newStores = Stores <$> (V.new 10 >>= newIORef) <*> newIORef M.empty

storeContains :: forall component. Storable component => Store component -> Entity -> IO Bool
storeGet :: forall component. Storable component => Store component -> Entity -> IO component
storePut :: forall component. Storable component => Store component -> Entity -> component -> IO ()
storeDelete :: forall component. Storable component => Store component -> Entity -> IO ()
storeFor :: forall component. Storable component => Store component -> (Entity -> component -> IO ()) -> IO ()
storeMembers :: forall component. Storable component => Store component -> IO Int
storeContains (Store set) entity = SV.contains set (coerce entity)
storeGet (Store set) entity = SV.unsafeLookup set (coerce entity)
storePut (Store set) entity val = SV.insert set (coerce entity) val
storeDelete (Store set) entity = SV.remove set (coerce entity)
storeFor (Store set) f = SV.for set (coerce f)
storeMembers (Store set) = SV.size set
{-# INLINE storeContains #-}
{-# INLINE storeGet #-}
{-# INLINE storePut #-}
{-# INLINE storeDelete #-}
{-# INLINE storeFor #-}

{-# INLINE storeMembers #-}