{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Internal.Query where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Data (Proxy (..))
import Data.IORef
import Hex.Internal.Component
import Hex.Internal.Entity
import Hex.Internal.World (World (..), worldComponentStorage)

newtype Query i m s = Query (i -> m s)

data QueryHead components = QueryGet

newtype QueryBody a = QueryBody {runQueryBody :: ReaderT (World, Entity) IO a} deriving (Functor, Applicative, Monad, MonadIO)

qWorld :: QueryBody World
qWorld = fmap fst qAsk
{-# INLINE qWorld #-}

qEntity :: QueryBody Entity
qEntity = fmap snd qAsk
{-# INLINE qEntity #-}

qAsk :: QueryBody (World, Entity)
qAsk = QueryBody $ ask
{-# INLINE qAsk #-}

worldQuery_ :: forall components m a. (MonadIO m, MPS components) => World -> (components -> QueryBody ()) -> m ()
worldQuery_ w f = liftIO $
  multiFor @(S components) @components w $ \e -> do
    comps <- multiGet @(S components) w e
    runReaderT (runQueryBody (f comps)) (w, e)
{-# INLINE worldQuery_ #-}

worldQuery :: forall components m a. (Monoid a, MonadIO m, MPS components) => World -> (components -> QueryBody a) -> m a
worldQuery w f = liftIO $ do
  ref <- newIORef mempty
  multiFor @(S components) @components w $ \e -> do
    comps <- multiGet @(S components) w e
    a <- runReaderT (runQueryBody (f comps)) (w, e)
    modifyIORef' ref (<> a)
  readIORef ref
{-# INLINE worldQuery #-}

qPut :: forall components. MPS components => components -> QueryBody ()
qPut components = qAsk >>= \(w, e) -> liftIO $ multiPut @(S components) w e components
{-# INLINE qPut #-}

qDelete :: forall components m. (MPS components, MonadIO m) => QueryBody ()
qDelete = qAsk >>= \(w, e) -> liftIO $ multiDelete @(S components) @components w e
{-# INLINE qDelete #-}

type family SingleComponent a where
  SingleComponent (a, b) = False
  SingleComponent _ = True

data MultiComponentParams components = MCParams Bool components

class MultiComponent (f :: Bool) components where
  multiContains :: World -> Entity -> IO Bool
  multiGet :: World -> Entity -> IO components
  multiPut :: World -> Entity -> components -> IO ()
  multiDelete :: World -> Entity -> IO ()
  multiFor :: World -> (Entity -> IO ()) -> IO ()
  multiMembers :: World -> IO Int

type MP = MultiComponent

type MPS a = MP (S a) a

type S a = SingleComponent a

instance Component a => MultiComponent True a where
  multiContains w e = worldComponentStorage @a w >>= flip storeContains e
  multiGet w e = worldComponentStorage @a w >>= flip storeGet e
  multiPut w e c = worldComponentStorage w >>= \s -> storePut s e c
  multiDelete w e = worldComponentStorage @a w >>= flip storeDelete e
  multiFor w f = worldComponentStorage @a w >>= flip storeFor f
  multiMembers w = worldComponentStorage @a w >>= storeMembers
  {-# INLINE multiContains #-}
  {-# INLINE multiGet #-}
  {-# INLINE multiPut #-}
  {-# INLINE multiDelete #-}
  {-# INLINE multiFor #-}
  {-# INLINE multiMembers #-}

instance (MPS a, MPS b) => MultiComponent False (a, b) where
  multiContains w e = (&&) <$> multiContains @(S a) @a w e <*> multiContains @(S b) @b w e
  multiGet w e = (,) <$> multiGet @(S a) @a w e <*> multiGet @(S b) @b w e
  multiPut w e (a, b) = multiPut @(S a) w e a *> multiPut @(S b) w e b
  multiDelete w e = multiDelete @(S a) @a w e *> multiDelete @(S b) @b w e
  multiFor w f = do
    amount1 <- multiMembers @(S a) @a w
    amount2 <- multiMembers @(S b) @b w
    if amount1 > amount2
      then multiFor @(S b) @b w $ \e -> do
        contained <- multiContains @(S a) @a w e
        when contained $ f e
      else multiFor @(S b) @b w $ \e -> do
        contained <- multiContains @(S a) @a w e
        when contained $ f e
  multiMembers w = min <$> multiMembers @(S a) @a w <*> multiMembers @(S b) @b w
  {-# INLINE multiContains #-}
  {-# INLINE multiGet #-}
  {-# INLINE multiPut #-}
  {-# INLINE multiDelete #-}
  {-# INLINE multiFor #-}
  {-# INLINE multiMembers #-}

instance (MPS a, MPS b, MPS c) => MultiComponent False (a, b, c) where
  multiContains w e = (&&) <$> ((&&) <$> multiContains @(S a) @a w e <*> multiContains @(S b) @b w e) <*> multiContains @(S c) @c w e
  multiGet w e = (,,) <$> multiGet @(S a) @a w e <*> multiGet @(S b) @b w e <*> multiGet @(S c) @c w e
  multiPut w e (a, b, c) = multiPut @(S a) w e a *> multiPut @(S b) w e b *> multiPut @(S c) @c w e c
  multiDelete w e = multiDelete @(S a) @a w e *> multiDelete @(S b) @b w e *> multiDelete @(S c) @c w e
  multiFor w f = do
    amountA <- multiMembers @(S a) @a w
    amountB <- multiMembers @(S b) @b w
    amountC <- multiMembers @(S c) @c w
    if amountA > amountB
      then
        if amountB > amountC
          then multiFor @(S c) @c w $ \e -> do
            containedA <- multiContains @(S a) @a w e
            containedB <- multiContains @(S b) @b w e
            when (containedA && containedB) $ f e
          else multiFor @(S b) @b w $ \e -> do
            containedA <- multiContains @(S a) @a w e
            containedC <- multiContains @(S c) @c w e
            when (containedA && containedC) $ f e
      else
        if amountA > amountC
          then multiFor @(S c) @c w $ \e -> do
            containedA <- multiContains @(S a) @a w e
            containedB <- multiContains @(S b) @b w e
            when (containedA && containedB) $ f e
          else multiFor @(S a) @a w $ \e -> do
            containedB <- multiContains @(S b) @b w e
            containedC <- multiContains @(S c) @c w e
            when (containedB && containedC) $ f e
  multiMembers w = min <$> (min <$> multiMembers @(S a) @a w <*> multiMembers @(S b) @b w) <*> multiMembers @(S c) @c w
  {-# INLINE multiContains #-}
  {-# INLINE multiGet #-}
  {-# INLINE multiPut #-}
  {-# INLINE multiDelete #-}
  {-# INLINE multiFor #-}
  {-# INLINE multiMembers #-}
