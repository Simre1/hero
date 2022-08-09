{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Hex.Internal.Component where

-- import Data.HashTable.IO as H

import Control.Monad.IO.Class
import Data.IORef
-- import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Vector.Mutable qualified as V
import GHC.IO
import Hex.Internal.Component.ComponentId
import Hex.Internal.Entity
import Language.Haskell.TH
import Type.Reflection
import Unsafe.Coerce

class ComponentAmount where
  componentAmount :: Int

class Component component where
  componentStorage :: MaxEntities -> IO (Store component)
  componentId :: Int

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

newtype Stores = Stores (V.IOVector WrappedStorage)

addStorage :: forall component. Component component => Stores -> Store component -> IO ()
addStorage (Stores ref) store =
  let id = componentId @component
   in V.unsafeWrite ref id (WrappedStorage $ unsafeCoerce store)

addComponentStorage :: forall component. (Component component) => Stores -> MaxEntities -> IO ()
addComponentStorage stores@(Stores mapRef) max = do
  store <- componentStorage @component max
  addStorage stores store

getStorage :: forall component. Component component => Stores -> IO (Maybe (Store component))
getStorage (Stores ref) = unsafeCoerce <$> V.readMaybe ref (componentId @component)

getComponentStorage :: forall component. (Component component) => Stores -> MaxEntities -> IO (Store component)
getComponentStorage stores@(Stores ref) max = do
  maybeStorage <- getStorage stores
  case maybeStorage of
    Just store -> pure store
    Nothing -> do
      store <- componentStorage max
      addStorage stores store
      pure store

newStores :: ComponentAmount => IO Stores
newStores = Stores <$> V.new componentAmount

storeContains :: forall component. Store component -> Entity -> IO Bool
storeGet :: forall component. Store component -> Entity -> IO component
storePut :: forall component. Store component -> Entity -> component -> IO ()
storeDelete :: forall component. Store component -> Entity -> IO ()
storeFor :: forall component. Store component -> (Entity -> component -> IO ()) -> IO ()
storeMembers :: forall component. Store component -> IO Int
storeContains (Store store) = storeClassContains store

storeGet (Store store) = storeClassGet store

storePut (Store store) = storeClassPut store

storeDelete (Store store) = storeClassDelete store

storeFor (Store store) = storeClassFor store

storeMembers (Store store) = storeClassMembers store