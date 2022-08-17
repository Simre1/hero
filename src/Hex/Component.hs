{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Hex.Component where

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
import Hex.Entity (Entity, MaxEntities (MaxEntities))
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

newtype ComponentId = ComponentId {unwrapComponentId :: Int} deriving (Show)

class (ComponentStore component (Store component), Typeable component) => Component component where
  type Store component :: Type -> Type
  liveEntities :: Maybe Word32
  liveEntities = Nothing

type Store' component = Store component component

data MakeStore = MakeStore {maxGlobalEntities :: Word32, maxComponentEntities :: Word32}

class ComponentStore component store where
  makeStore :: MakeStore -> IO (store component)
  storeContains :: store component -> Entity -> IO Bool
  storeGet :: store component -> Entity -> IO component
  storePut :: store component -> Entity -> component -> IO ()
  storeDelete :: store component -> Entity -> IO ()
  storeFor :: store component -> (Entity -> component -> IO ()) -> IO ()
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

getComponentId :: forall component. (ComponentStore component (Store component), Component component) => Stores -> MaxEntities -> IO ComponentId
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
