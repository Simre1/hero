{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Hero.Component where

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
import Hero.Entity (Entity, MaxEntities (MaxEntities))
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.IO.Class

-- | ComponentId is unique to a component within the context of a World.
newtype ComponentId = ComponentId {unwrapComponentId :: Int} deriving (Show)

-- | A component is a Haskell datatype usually containing raw data.
-- For example `data Position = Position Float Float`
class (ComponentMakeStore component (Store component), Typeable component) => Component component where
  type Store component :: Type -> Type
  liveEntities :: Maybe Word32
  liveEntities = Nothing

type Store' component = Store component component

data MakeStore = MakeStore {maxGlobalEntities :: Word32, maxComponentEntities :: Word32}

class ComponentMakeStore component store where
  makeStore :: MakeStore -> IO (store component)

class ComponentGet component store where
  storeContains :: store component -> Entity -> IO Bool
  storeGet :: store component -> Entity -> IO component

class ComponentPut component store where
  storePut :: store component -> Entity -> component -> IO ()

class ComponentDelete component store where
  storeDelete :: store component -> Entity -> IO ()

class ComponentGet component store => ComponentIterate component store where
  storeFor :: MonadIO m => store component -> (Entity -> component -> m ()) -> m ()
  storeMembers :: store component -> IO Int

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

addComponentStore :: forall component. (Component component) => Stores -> MaxEntities -> IO ComponentId
addComponentStore stores@(Stores storeVec mappings) (MaxEntities max) = do
  store <- makeStore @component $ MakeStore max (fromMaybe max (liveEntities @component))
  addStore @component stores store

getStore :: forall component. Stores -> ComponentId -> IO (Store' component)
getStore (Stores storeVecRef _) componentId =
  readIORef storeVecRef >>= \vec -> unwrapWrappedStorage @component <$> V.unsafeRead vec (unwrapComponentId componentId)

getComponentId :: forall component. (ComponentMakeStore component (Store component), Component component) => Stores -> MaxEntities -> IO ComponentId
getComponentId stores@(Stores _ mappingsRef) (MaxEntities max) = do
  maybeComponent <- M.lookup (someTypeRep $ Proxy @component) <$> readIORef mappingsRef
  case maybeComponent of
    Just componentId -> pure componentId
    Nothing -> do
      store <- makeStore @component $ MakeStore max (fromMaybe max (liveEntities @component))
      componentId <- addStore @component stores store
      pure componentId

newStores :: IO Stores
newStores = Stores <$> (V.new 10 >>= newIORef) <*> newIORef M.empty
