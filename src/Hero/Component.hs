{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Hero.Component
  ( -- * Component

    -- To make a component of a datatype, you have to decide on the underlying datastructure.
    -- Most likely, SparseSetStorableStore or SparseSetBoxedStore are the way to go.
    Component (..),

    -- * Store

    -- Store have different capabilities and not every store implements every operation.
    -- For example, you cannot set or delete the Entity component.
    ComponentId,
    ComponentMakeStore (..),
    ComponentGet (..),
    ComponentPut (..),
    ComponentDelete (..),
    ComponentIterate (..),
    Store',
    Stores,
    newStores,
    getComponentId,
    getStore,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.IORef
  ( IORef,
    modifyIORef,
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Vector.Mutable qualified as V
import Data.Vector.Storable (Storable)
import Data.Word (Word32)
import Hero.Entity (Entities, Entity, MaxEntities (MaxEntities))
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

-- | 'ComponentId' is unique to a component within the context of a World. Do not
-- use the same 'ComponentId' with different worlds.
newtype ComponentId = ComponentId {unwrapComponentId :: Int} deriving (Show, Eq)

-- | A component is a Haskell datatype usually containing raw data.
-- For example `data Position = Position Float Float`
class Typeable component => Component component where
  type Store component :: Type -> Type
  makeStore :: MaxEntities -> IO (Store' component)
  default makeStore :: ComponentMakeStore component (Store component) => MaxEntities -> IO (Store' component)
  makeStore = componentMakeStore

type Store' component = Store component component

-- | The default implementation for 'makeStore'. It can be overriden by providing a
-- 'makeStore' method for a component.
class ComponentMakeStore component store where
  componentMakeStore :: MaxEntities -> IO (store component)

class ComponentGet component store where
  componentContains :: store component -> Entity -> IO Bool
  componentGet :: store component -> Entity -> IO component

class ComponentPut component store where
  componentPut :: store component -> Entity -> component -> IO ()

class ComponentDelete component store where
  componentDelete :: store component -> Entity -> IO ()

class ComponentGet component store => ComponentIterate component store where
  componentIterate :: MonadIO m => Entities -> store component -> (Entity -> component -> m ()) -> m ()
  componentMembers :: Entities -> store component -> IO Int

newtype WrappedStorage = WrappedStorage (forall component. Store' component)

unwrapWrappedStorage :: forall component. WrappedStorage -> Store' component
unwrapWrappedStorage (WrappedStorage s) = s @component

data Stores = Stores (IORef (V.IOVector WrappedStorage)) (IORef (M.Map SomeTypeRep ComponentId))

addStore :: forall component. Component component => Stores -> Store' component -> IO ComponentId
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

addComponentStore :: forall (component :: Type). Component component => Stores -> MaxEntities -> IO ComponentId
addComponentStore stores@(Stores storeVec mappings) max = do
  store <- makeStore @component max
  addStore @component stores store

-- | Gets the store associated within a component id. Remember that component ids must be used with the world
-- they were created with.
getStore :: forall component. Stores -> ComponentId -> IO (Store' component)
getStore (Stores storeVecRef _) componentId =
  readIORef storeVecRef >>= \vec -> unwrapWrappedStorage @component <$> V.unsafeRead vec (unwrapComponentId componentId)

-- | Gets the component id for a component. It sets up the component store if it does not exist yet within the given world.
getComponentId :: forall (component :: Type). Component component => Stores -> MaxEntities -> IO ComponentId
getComponentId stores@(Stores _ mappingsRef) max = do
  maybeComponent <- M.lookup (someTypeRep $ Proxy @component) <$> readIORef mappingsRef
  case maybeComponent of
    Just componentId -> pure componentId
    Nothing -> do
      componentId <- addComponentStore @component stores max
      pure componentId

-- | 'Stores' holds all component stores.
newStores :: IO Stores
newStores = Stores <$> (V.new 10 >>= newIORef) <*> newIORef M.empty
