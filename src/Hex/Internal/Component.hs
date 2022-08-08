{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module Hex.Internal.Component where

-- import Data.HashTable.IO as H

import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Proxy
import Hex.Internal.Entity
import Type.Reflection
import Unsafe.Coerce

class Typeable component => Component component where
  componentStorage :: MaxEntities -> IO (Store component)

newtype WrappedStorage = WrappedStorage (forall component. Store component)

class StoreClass store component where
  storeClassContains :: store component -> Entity -> IO Bool
  storeClassGet :: store component -> Entity -> IO component
  storeClassPut :: store component -> Entity -> component -> IO ()
  storeClassDelete :: store component -> Entity -> IO ()
  storeClassFor :: store component -> (Entity -> component -> IO ()) -> IO ()
  storeClassMembers :: store component -> IO Int

data Store component where
  Store :: StoreClass store component => store component -> Store component 

-- newtype Stores = Stores (H.BasicHashTable SomeTypeRep WrappedStorage)

newtype Stores = Stores (IORef (M.Map SomeTypeRep WrappedStorage))

addStorage :: forall component. Typeable component => Stores -> Store component -> IO ()
addStorage (Stores mapRef) store =
  modifyIORef' mapRef $
    M.insert
      (someTypeRep (Proxy @component))
      (WrappedStorage $ unsafeCoerce store)

addComponentStorage :: forall component. (Component component, Typeable component) => Stores -> MaxEntities -> Proxy component -> IO ()
addComponentStorage stores@(Stores mapRef) max _ = do
  store <- componentStorage @component max
  addStorage stores store

getStorage :: forall component. Typeable component => Stores -> IO (Maybe (Store component))
getStorage (Stores mapRef) =
  unsafeCoerce $ M.lookup (someTypeRep (Proxy @component)) <$> readIORef mapRef

getComponentStorage :: forall component. (Typeable component, Component component) => Stores -> MaxEntities -> IO (Store component)
getComponentStorage stores@(Stores mapRef) max = do
  map <- readIORef mapRef
  let maybeStorage = M.lookup (someTypeRep (Proxy @component)) map
  case maybeStorage of
    Just store -> pure $ unsafeCoerce store
    Nothing -> do
      store <- componentStorage max
      addStorage stores store
      pure store

newStores :: IO Stores
newStores = Stores <$> newIORef M.empty

storeContains :: forall component. Store component -> Entity -> IO Bool
storeGet :: forall component. Store component -> Entity -> IO component
storePut :: forall component. Store component -> Entity -> component -> IO ()
storeDelete :: forall component. Store component -> Entity -> IO ()
storeFor :: forall component. Store component -> (Entity -> component -> IO ()) -> IO ()
storeMembers :: forall component. Store component -> IO Int
storeContains (Store store) = storeClassContains store
storeGet (Store store) =  storeClassGet store 
storePut (Store store) = storeClassPut store 
storeDelete (Store store) = storeClassDelete store 
storeFor (Store store) = storeClassFor store 
storeMembers (Store store) = storeClassMembers store 