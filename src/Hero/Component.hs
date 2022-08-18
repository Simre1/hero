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
    ComponentStore (..),
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
    eachStore,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Constraint
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
newtype ComponentId = ComponentId {unwrapComponentId :: Int} deriving (Show, Eq, Ord)

-- | A component is a Haskell datatype usually containing raw data.
-- For example `data Position = Position Float Float`
class (Typeable component, ComponentStore component (Store component)) => Component component where
  type Store component :: Type -> Type
  makeStore :: MaxEntities -> IO (Store' component)
  default makeStore :: ComponentMakeStore component (Store component) => MaxEntities -> IO (Store' component)
  makeStore = componentMakeStore

type Store' component = Store component component

data Stores
  = Stores
      (IORef (V.IOVector (WrappedStore, WrappedDict)))
      (IORef (M.Map SomeTypeRep ComponentId))

-- | Contains methods that every store must have.
class ComponentStore component store where
  componentEntityDelete :: store component -> Entity -> IO ()

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
  componentIterate :: MonadIO m => store component -> (Entity -> component -> m ()) -> m ()
  componentMembers :: store component -> IO Int

newtype WrappedStore = WrappedStore (forall component. Store' component)

newtype WrappedDict = WrappedDict (forall component. Dict (ComponentStore component (Store component)))

unwrapStore :: forall component. WrappedStore -> Store' component
unwrapStore (WrappedStore s) = s @component

unwrapDict :: forall component. WrappedDict -> Dict (ComponentStore component (Store component))
unwrapDict (WrappedDict s) = s @component

getDict :: forall component. Component component => WrappedDict
getDict = WrappedDict $ unsafeCoerce $ Dict @(ComponentStore component (Store component))

addStore :: forall component. Component component => Stores -> Store' component -> IO ComponentId
addStore (Stores storeVecRef mappingsRef) store = do
  mappings <- readIORef mappingsRef
  storeVec <- readIORef storeVecRef
  let rep = someTypeRep $ Proxy @component
      maybeMapping = M.lookup rep mappings
      wrappedStore = (WrappedStore $ unsafeCoerce store)
      wrappedDict = getDict @component
  case maybeMapping of
    Nothing -> do
      let newId = M.size mappings
          storeSize = V.length storeVec
      if newId < storeSize
        then V.unsafeWrite storeVec newId (wrappedStore, wrappedDict)
        else do
          newStoreVec <- V.grow storeVec (storeSize `quot` 2)
          V.unsafeWrite newStoreVec newId (wrappedStore, wrappedDict)
          writeIORef storeVecRef newStoreVec
      modifyIORef mappingsRef $ M.insert rep (ComponentId newId)
      pure $ ComponentId newId
    Just i -> do
      V.unsafeWrite storeVec (unwrapComponentId i) (wrappedStore, wrappedDict) *> pure i

addComponentStore :: forall (component :: Type). Component component => Stores -> MaxEntities -> IO ComponentId
addComponentStore stores@(Stores storeVec mappings) max = do
  store <- makeStore @component max
  addStore @component stores store

-- | Gets the store associated within a component id. Remember that component ids must be used with the world
-- they were created with.
getStore :: forall component. Stores -> ComponentId -> IO (Store' component)
getStore (Stores storeVecRef _) componentId =
  readIORef storeVecRef >>= \vec -> unwrapStore @component . fst <$> V.unsafeRead vec (unwrapComponentId componentId)

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

-- | Do something on each store
eachStore :: Stores -> (forall component. ComponentStore component (Store component) => Store' component -> IO ()) -> IO ()
eachStore (Stores storeVec mappingsRef) f = do
  stores <- readIORef storeVec
  mappings <- readIORef mappingsRef
  forM_ [0 .. M.size mappings - 1] $ \i -> do
    (wrappedStore, wrappedDict) <- V.unsafeRead stores i
    g (unwrapDict wrappedDict) (unwrapStore wrappedStore)
  where
    g :: Dict (ComponentStore component (Store component)) -> Store' component -> IO ()
    g d s = withDict d $ f s
