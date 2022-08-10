{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
module Hex.Internal.NewQuery where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Hex.Internal.Component
import Hex.Internal.Entity
import Hex.Internal.World
import Control.Category
import Control.Arrow
import Prelude hiding ((.))


data Query m i o where
  QueryPut :: Component a => Query m a ()
  QueryDelete :: Component a => Query m a ()
  QueryConnect :: Query m a b -> Query m b c -> Query m a c
  QueryMap :: (a -> m b) -> Query m a b
  QueryPar :: Query m i1 o1 -> Query m i2 o2 -> Query m (i1,i2) (o1,o2)

data System m i o where
  SystemExec :: Component i1 => Query m (i1, i2) () -> System m i2 ()
  SystemSingle :: Query m i o -> Entity -> System m i o
  SystemFor :: (Component i1, Monoid o) => Query m (i1,i2) o -> System m i2 o
  SystemNewEntity :: Component c => System m c Entity

instance Applicative m => Category (Query m) where
  id = QueryMap pure
  (.) = flip QueryConnect

instance Applicative m => Arrow (Query m) where
  arr f = QueryMap (pure . f)
  (***) = QueryPar

newtype CompiledQuery m i a = CompiledQuery (i -> m a)

data QueryContext = QueryContext ()

compileSystem :: World -> System IO i a -> IO (i -> IO a)
compileSystem w q = case q of
  SystemExec @i1 q -> do
    cQ <- compileQuery w q
    store <- worldComponent @i1 w
    pure $ \i2 -> storeFor store $ \e i1 -> cQ (i1,i2) e
  SystemSingle q e -> do
    cQ <- compileQuery w q
    pure $ \i -> cQ i e
  SystemFor @i1 @o q -> do
    cQ <- compileQuery w q
    store <- worldComponent @i1 w
    pure $ \i2 -> do
      ref <- newIORef (mempty :: o)
      storeFor store $ \e i1 -> do
        o <- cQ (i1,i2) e
        modifyIORef ref (<> o)
      readIORef ref
  SystemNewEntity @c -> do
    store <- worldComponent @c w
    pure $ \c -> do
      e <- worldNewEntity w
      storePut store e c
      pure e
{-# INLINE compileSystem #-}

compileQuery :: World -> Query IO i a -> IO (i -> Entity -> IO a)
compileQuery w q = case q of
  QueryPut @p -> do
    store <- worldComponent @p w
    pure $ \i e -> liftIO $ storePut store e i
  QueryDelete @d -> do
    store <- worldComponent @d w
    pure $ \i e -> liftIO $ storeDelete store e
  QueryConnect q1 q2 -> do
    cQ1 <- compileQuery w q1
    cQ2 <- compileQuery w q2
    pure $ \a e -> cQ1 a e >>= \b -> cQ2 b e
  QueryMap f -> pure $ \a e -> (f a)
  QueryPar q1 q2 -> do
    cQ1 <- compileQuery w q1
    cQ2 <- compileQuery w q2
    pure $ \(i1,i2) e -> (,) <$> cQ1 i1 e <*> cQ2 i2 e
{-# INLINE compileQuery #-}

cmap :: (Component a, Component b, Applicative m) => (a -> b) -> System m () ()
cmap f = SystemExec (arr (f . fst) >>> QueryPut)
{-# INLINE cmap #-}

type QC a = QueryComponent (S a) a

class QueryComponent (f :: Bool) component where
  queryContains :: World -> IO (Entity -> IO Bool)
  queryGet :: World -> IO (Entity -> IO component)
  -- queryPut :: World -> Entity -> component -> IO ()
  -- queryDelete :: World -> Entity -> IO ()
  -- queryFor :: World -> (Entity -> component -> IO ()) -> IO ()
  -- queryMembers :: World -> IO Int

-- type MP = MultiComponent

-- type MPS a = MP (S a) a

type family S a where
  S () = False
  S (a, b) = False
  S (a, b, c) = False
  S _ = True

-- instance QueryComponent False () where
--   multiContains w e = pure False
--   multiGet w e = pure ()
--   multiPut w e c = pure ()
--   multiDelete w e = pure ()
--   multiFor w f = pure ()
--   multiMembers w = pure 0
--   {-# INLINE multiContains #-}
--   {-# INLINE multiGet #-}
--   {-# INLINE multiPut #-}
--   {-# INLINE multiDelete #-}
--   {-# INLINE multiFor #-}
--   {-# INLINE multiMembers #-}
