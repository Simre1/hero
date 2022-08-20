{-# LANGUAGE AllowAmbiguousTypes #-}

module Hero.Component.Store.AllStores where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Constraint (Dict (..), withDict)
import Data.Data (Proxy (Proxy))
import Data.IORef
  ( IORef,
    modifyIORef,
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Kind (Type)
import Data.Map.Strict qualified as M
import Data.Vector.Mutable qualified as V
import Hero.Component.Component
  ( Component (..),
    ComponentId (ComponentId),
    ComponentStore,
    Store',
  )
import Hero.Entity (MaxEntities)
import Type.Reflection (SomeTypeRep, someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

data AllStores
  = AllStores
      (IORef (V.IOVector (WrappedStore, WrappedDict)))
      (IORef (M.Map SomeTypeRep ComponentId))

newtype WrappedStore = WrappedStore (forall component. Store' component)

newtype WrappedDict = WrappedDict (forall component. Dict (ComponentStore component (Store component)))

unwrapStore :: forall component. WrappedStore -> Store' component
unwrapStore (WrappedStore s) = s @component

unwrapDict :: forall component. WrappedDict -> Dict (ComponentStore component (Store component))
unwrapDict (WrappedDict s) = s @component

getDict :: forall component. Component component => WrappedDict
getDict = WrappedDict $ unsafeCoerce $ Dict @(ComponentStore component (Store component))

addStore :: forall component. Component component => AllStores -> Store' component -> IO ComponentId
addStore (AllStores storeVecRef mappingsRef) store = do
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
    Just i ->
      error $
        "You tried to create a store for "
          <> show (someTypeRep (Proxy @component))
          <> ", but it already exists. You may not create stores multiple times!"

-- | Gets the store associated within a component id. Remember that component ids must be used with the world
-- they were created with.
getStore :: forall component. AllStores -> ComponentId -> IO (Store' component)
getStore (AllStores storeVecRef _) componentId =
  readIORef storeVecRef >>= \vec -> unwrapStore @component . fst <$> V.unsafeRead vec (coerce componentId)

-- | Gets the component id for a component. It tries to set up the component store if it can does not exist yet and can be created automatically.
-- If a store does not exist and it cannot be created, then this function will error.
getComponentId :: forall (component :: Type). (Component component) => AllStores -> MaxEntities -> IO ComponentId
getComponentId stores@(AllStores _ mappingsRef) max = do
  maybeComponent <- M.lookup (someTypeRep $ Proxy @component) <$> readIORef mappingsRef
  case maybeComponent of
    Just componentId -> pure componentId
    Nothing ->
      case (createStoreAutomatically @component) of
        Nothing ->
          error $
            "Tried to get a store for component "
              <> show (someTypeRep (Proxy @component))
              <> ", but it has not been created yet. It also cannot be automatically created. You need to create the store before using it."
        Just autoCreate -> do
          store <- autoCreate max
          cId <- addStore stores store
          pure cId

-- | 'AllStores' holds all component stores.
createStores :: IO AllStores
createStores = AllStores <$> (V.new 10 >>= newIORef) <*> newIORef M.empty

-- | Do something on each store
eachStore :: AllStores -> (forall component. ComponentStore component (Store component) => Store' component -> IO ()) -> IO ()
eachStore (AllStores storeVec mappingsRef) f = do
  stores <- readIORef storeVec
  mappings <- readIORef mappingsRef
  forM_ [0 .. M.size mappings - 1] $ \i -> do
    (wrappedStore, wrappedDict) <- V.unsafeRead stores i
    g (unwrapDict wrappedDict) (unwrapStore wrappedStore)
  where
    g :: Dict (ComponentStore component (Store component)) -> Store' component -> IO ()
    g d s = withDict d $ f s