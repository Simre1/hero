module Hex.Internal.Component where

import Data.HashTable.IO as H
import Data.Proxy
import Hex.Internal.Entity
import Type.Reflection
import Unsafe.Coerce
import Control.Monad.IO.Class

class Typeable component => Component component where
  componentStorage :: MaxEntities -> IO (Store component)
  
newtype WrappedStorage = WrappedStorage (forall component. Store component)

data Store component = Store
  { storeContains :: Entity -> IO Bool,
    storeGet :: Entity -> IO component,
    storePut :: Entity -> component -> IO (),
    storeDelete :: Entity -> IO (),
    storeFor :: forall m. MonadIO m => (m () -> IO ()) -> (Entity -> component -> m ()) -> IO (),
    storeMembers :: IO Int
  }

newtype Stores = Stores (H.BasicHashTable SomeTypeRep WrappedStorage)

addStorage :: forall component. Typeable component => Stores -> Store component -> IO ()
addStorage (Stores table) store =
  H.insert
    table
    (someTypeRep (Proxy @component))
    (WrappedStorage $ unsafeCoerce store)

addComponentStorage :: forall component. (Component component, Typeable component) => Stores -> MaxEntities -> Proxy component -> IO ()
addComponentStorage (Stores table) max _ = do
  store <- componentStorage @component max
  H.insert
    table
    (someTypeRep (Proxy @component))
    (WrappedStorage $ unsafeCoerce store)

getStorage :: forall component. Typeable component => Stores -> IO (Maybe (Store component))
getStorage (Stores table) =
  unsafeCoerce $
    H.lookup table (someTypeRep (Proxy @component))

getComponentStorage :: forall component. (Typeable component, Component component) => Stores -> MaxEntities -> IO (Store component)
getComponentStorage stores@(Stores table) max = do
  maybeStorage <- H.lookup table (someTypeRep (Proxy @component))
  case maybeStorage of
    Just store -> pure $ unsafeCoerce store
    Nothing -> do
      store <- componentStorage max
      addStorage stores store
      pure store

newStores :: IO Stores
newStores = Stores <$> H.new
