module Hex.Internal.Component where

-- import Data.HashTable.IO as H
import qualified Data.Map.Strict as M
import Data.Proxy
import Hex.Internal.Entity
import Type.Reflection
import Unsafe.Coerce
import Control.Monad.IO.Class
import Data.IORef

class Typeable component => Component component where
  componentStorage :: MaxEntities -> IO (Store component)
  
newtype WrappedStorage = WrappedStorage (forall component. Store component)

data Store component = Store
  { storeContains :: Entity -> IO Bool,
    storeGet :: Entity -> IO component,
    storePut :: Entity -> component -> IO (),
    storeDelete :: Entity -> IO (),
    storeFor :: (Entity -> IO ()) -> IO (),
    storeMembers :: IO Int
  }

-- newtype Stores = Stores (H.BasicHashTable SomeTypeRep WrappedStorage)

newtype Stores = Stores (IORef (M.Map SomeTypeRep WrappedStorage))

addStorage :: forall component. Typeable component => Stores -> Store component -> IO ()
addStorage (Stores mapRef) store =
  modifyIORef' mapRef $ M.insert
      (someTypeRep (Proxy @component))
      (WrappedStorage $ unsafeCoerce store)

addComponentStorage :: forall component. (Component component, Typeable component) => Stores -> MaxEntities -> Proxy component -> IO ()
addComponentStorage (Stores mapRef) max _ = do
  store <- componentStorage @component max
  modifyIORef' mapRef $ M.insert
      (someTypeRep (Proxy @component))
      (WrappedStorage $ unsafeCoerce store)

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
