module Hex.Internal.System where

import Hex.Internal.World
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Hex.Internal.Entity
import Hex.Internal.Component
import Control.Monad.Trans.Class
import UnliftIO

newtype System m a = System (ReaderT World m a) deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

askWorld :: Monad m => System m World
askWorld = System ask
{-# Inline askWorld #-}


askStore :: forall component m.  (MonadIO m, Component component) => Monad m => System m (Store component)
askStore = askWorld >>= liftIO . worldComponentStorage @component
{-# Inline askStore #-}

cmap :: (Component a, Component b, MonadIO m) => (a -> b) -> System m ()
cmap f = do
  aStore <- askStore
  bStore <- askStore
  liftIO $ storeFor aStore id $ \e a -> 
    storePut bStore e $ f a 
{-# Inline cmap #-}

cmapM :: (MonadIO m, Component a, Component b, MonadIO m, MonadUnliftIO m) => (a -> m b) -> System m ()
cmapM f = do
  aStore <- askStore
  bStore <- askStore
  lift $ withRunInIO $ \unlift -> storeFor aStore unlift $ \e a -> do
    b <- f a
    liftIO $ storePut bStore e b  
{-# Inline cmapM #-}


newEntity :: MonadIO m => System m Entity
newEntity = askWorld >>= liftIO . worldNewEntity  
{-# Inline newEntity #-}


putEntity :: Component a => MonadIO m => Entity -> a -> System m ()
putEntity entity a = do
  aStore <- askStore
  liftIO $ storePut aStore entity a
{-# Inline putEntity #-}

runSystem :: World -> System m a -> m a
runSystem w (System r) = runReaderT r w
{-# Inline runSystem #-}
