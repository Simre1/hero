{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.System where

import Control.Arrow (Arrow (arr, (***)))
import Control.Category (Category (..))
import Control.Monad
  ( Monad ((>>=)),
    void,
    when,
    (<$!>),
  )
import Data.Coerce (Coercible, coerce)
import Data.Functor ((<$>), (<&>))
import Data.IORef
  ( modifyIORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.Monoid
  ( Dual (Dual, getDual),
    Endo (Endo, appEndo),
    Monoid (mempty),
    (<>),
  )
import Hex.Component
  ( Component (Store),
    ComponentDelete (..),
    ComponentGet (..),
    ComponentIterate (..),
    ComponentPut (..),
    addStore,
  )
import Hex.Entity (Entity, entityAmount, forEntities)
import Hex.World
  ( World (worldEntities, worldStores),
    worldComponent,
    worldNewEntity,
  )
import Prelude hiding (id, (.))

newtype System m i o = System (World -> m (i -> m o))

instance Monad m => Category (System m) where
  id = System (\_ -> pure (pure . id))
  (System f1) . (System f2) = System $ \w -> do
    f1' <- f1 w
    f2' <- f2 w
    pure $ \i -> f2' i >>= f1'
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Monad m => Arrow (System m) where
  arr f = System (\_ -> pure (pure . f))
  (System f1) *** (System f2) = System $ \w -> do
    f1' <- f1 w
    f2' <- f2 w
    pure $ \(i1, i2) -> (,) <$> f1' i1 <*> f2' i2
  {-# INLINE arr #-}
  {-# INLINE (***) #-}

instance Applicative m => Functor (System m i) where
  fmap f (System makeS) = System $ \w -> do
    s <- makeS w
    pure $ \i -> fmap f (s i)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (System m i) where
  pure a = System $ \_ -> pure $ \_ -> pure a
  (System makeSF) <*> (System makeSV) = System $ \w -> do
    f <- makeSF w
    v <- makeSV w
    pure $ \i -> f i <*> v i
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

arrM :: Applicative m => (a -> m b) -> System m a b
arrM f = System $ \_ -> pure $ f
{-# INLINE arrM #-}

compileSystem :: System IO i o -> World -> IO (i -> IO o)
compileSystem (System f) w = f w
{-# INLINE compileSystem #-}

cmap ::
  forall a b m.
  (QCI a, QCP b) =>
  (a -> b) ->
  System IO () ()
cmap f =
  System
    ( \w -> do
        for <- queryFor @(S a) @a w
        put <- queryPut @(S b) @b w
        pure $ \_ -> for $ \e a -> put e $! (f a)
    )

cmapM :: forall a b m. (QCI a, QCP b) => (a -> IO b) -> System IO () ()
cmapM f =
  System
    ( \w -> do
        for <- queryFor @(S a) @a w
        put <- queryPut @(S b) @b w
        pure $ \_ -> for $ \e a -> f a >>= put e
    )

cfold :: forall a o m. (Monoid o, QCI a) => (a -> o) -> System IO () o
cfold f = System $ \w -> do
  for <- queryFor @(S a) @a w
  members <- queryMembers @(S a) @a w
  pure $! \i2 -> do
    ref <- newIORef (mempty :: o)
    for $! \e a -> do
      let o = f a
      modifyIORef' ref (<> o)
    readIORef ref

cfoldM :: forall a o m. (Monoid o, QCI a) => (a -> IO o) -> System IO () o
cfoldM f = System $ \w -> do
  for <- queryFor @(S a) @a w
  pure $! \i2 -> do
    ref <- newIORef (mempty :: o)
    for $! \e a -> do
      o <- f a
      modifyIORef ref (<> o)
    readIORef ref

cfoldr :: (QCI a) => (a -> b -> b) -> b -> System IO () b
cfoldr f b = fmap (($! b) . appEndo) $! cfold $! Endo #. f
{-# INLINE cfoldr #-}

cfoldl :: (QCI a) => (b -> a -> b) -> b -> System IO () b
cfoldl f b = fmap (($! b) . appEndo . getDual) $! cfold $! Dual . Endo . flip f
{-# INLINE cfoldl #-}

newEntity :: forall a. (QCP a) => System IO a Entity
newEntity = System $ \w -> do
  put <- queryPut @(S a) @a w
  pure $! \c -> do
    e <- worldNewEntity w
    put e c
    pure e
{-# INLINE newEntity #-}

queryExec :: forall i1 i2 o. (QCI i1, Monoid o) => Query IO (i1, i2) o -> System IO i2 o
queryExec (Query makeQ) = System $ \w -> do
  q <- makeQ w
  for <- queryFor @(S i1) @i1 w
  pure $ \i2 -> do
    ref <- newIORef (mempty :: o)
    for $! \e i1 -> do
      o <- q e (i1, i2)
      modifyIORef' ref (<> o)
    readIORef ref
{-# INLINE queryExec #-}

queryExec_ :: forall i o. (QCI i) => Query IO i o -> System IO () ()
queryExec_ (Query makeQ) = System $ \w -> do
  q <- makeQ w
  for <- queryFor @(S i) @i w
  pure $ \_ -> for (fmap (fmap void) q)
{-# INLINE queryExec_ #-}

querySingle_ :: forall i o. (QCG i) => Query IO i o -> World -> IO (Entity -> IO ())
querySingle_ (Query makeQ) w = do
  q <- makeQ w
  getValues <- queryGet @(S i) @i w
  contains <- queryContains @(S i) @i w
  pure $ \e -> do
    whenIO (contains e) $ do
      values <- getValues e
      q e values
      pure ()
{-# INLINE querySingle_ #-}

querySingle :: forall i1 i2 o. (QCG i1) => Query IO (i1, i2) o -> World -> IO (Entity -> i2 -> IO (Maybe o))
querySingle (Query makeQ) w = do
  q <- makeQ w
  getValues <- queryGet @(S i1) @i1 w
  contains <- queryContains @(S i1) @i1 w
  pure $ \e i2 -> do
    c <- (contains e)
    if c
      then do
        values <- getValues e
        Just <$> q e (values, i2)
      else pure Nothing
{-# INLINE querySingle #-}

newtype Query m i o = Query (World -> IO (Entity -> i -> IO o))

instance Applicative m => Category (Query m) where
  id = Query $ \_ -> pure $ \_ i -> pure i
  (Query makeS1) . (Query makeS2) = Query $ \w -> do
    s1 <- makeS1 w
    s2 <- makeS2 w
    pure $ \e i -> s2 e i >>= s1 e
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Applicative m => Arrow (Query m) where
  arr f = Query $ \_ -> pure $ \_ -> pure . f
  (Query makeS1) *** (Query makeS2) = Query $ \w -> do
    s1 <- makeS1 w
    s2 <- makeS2 w
    pure $ \e (i1, i2) -> (,) <$> s1 e i1 <*> s2 e i2
  {-# INLINE arr #-}
  {-# INLINE (***) #-}

qput :: forall i m. QCP i => Query IO i ()
qput = Query $ \w -> do
  put <- queryPut @(S i) @i w
  pure put
{-# INLINE qput #-}

qdelete :: forall i m. QCD i => Query IO i ()
qdelete = Query $ \w -> do
  delete <- queryDelete @(S i) @i w
  pure $! \e _ -> delete e
{-# INLINE qdelete #-}

type family S a where
  S () = False
  S Entity = False
  S (a, b) = False
  S (a, b, c) = False
  S _ = True

class QueryGet (f :: Bool) components where
  queryContains :: World -> IO (Entity -> IO Bool)
  queryGet :: World -> IO (Entity -> IO components)

class QueryPut (f :: Bool) components where
  queryPut :: World -> IO (Entity -> components -> IO ())

class QueryDelete (f :: Bool) components where
  queryDelete :: World -> IO (Entity -> IO ())

class QueryGet f components => QueryIterate (f :: Bool) components where
  queryFor :: World -> IO ((Entity -> components -> IO ()) -> IO ())
  queryMembers :: World -> IO (IO Int)

type QCG (a :: *) = QueryGet (S a) a

type QCP (a :: *) = QueryPut (S a) a

type QCD (a :: *) = QueryDelete (S a) a

type QCI (a :: *) = QueryIterate (S a) a

instance QueryGet False () where
  queryContains w = pure $! \_ -> pure True
  queryGet w = pure $! \_ -> pure ()
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}

instance QueryPut False () where
  queryPut w = pure $! \_ _ -> pure ()
  {-# INLINE queryPut #-}

instance QueryDelete False () where
  queryDelete w = pure $! \_ -> pure ()
  {-# INLINE queryDelete #-}

instance QueryIterate False () where
  queryFor w = do
    pure $! \f -> forEntities (worldEntities w) $! \e -> f e ()
  queryMembers w = do
    pure $! entityAmount (worldEntities w)
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

instance QueryGet False Entity where
  queryContains w = pure $! \_ -> pure True
  queryGet w = pure $! \e -> pure e
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}

instance QueryIterate False Entity where
  queryFor w = do
    pure $! \f -> forEntities (worldEntities w) $! \e -> f e e
  queryMembers w = do
    pure $! entityAmount (worldEntities w)
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

instance (Component a, ComponentGet a (Store a)) => QueryGet True a where
  queryContains w = worldComponent @a w <&> \s !e -> storeContains s e
  queryGet w = worldComponent @a w <&> \s !e -> storeGet s e
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}

instance (Component a, ComponentPut a (Store a)) => QueryPut True a where
  queryPut w = worldComponent w <&> \s !e c -> storePut s e c
  {-# INLINE queryPut #-}

instance (Component a, ComponentDelete a (Store a)) => QueryDelete True a where
  queryDelete w = worldComponent @a w <&> \s !e -> storeDelete s e
  {-# INLINE queryDelete #-}

instance (Component a, ComponentIterate a (Store a)) => QueryIterate True a where
  queryFor w = worldComponent @a w <&> \s f -> storeFor s f
  queryMembers w = worldComponent @a w <&> storeMembers
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

instance (QCG a, QCG b) => QueryGet False (a, b) where
  queryContains w = (\p1 p2 e -> (&&) <$!> p1 e <*> p2 e) <$!> queryContains @(S a) @a w <*> queryContains @(S b) @b w
  queryGet w = (\f1 f2 e -> (,) <$!> f1 e <*> f2 e) <$!> queryGet @(S a) @a w <*> queryGet @(S b) @b w
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}

instance (QCP a, QCP b) => QueryPut False (a, b) where
  queryPut w = do
    putA <- queryPut @(S a) w
    putB <- queryPut @(S b) w
    pure $! \e (a, b) -> putA e a *> putB e b
  {-# INLINE queryPut #-}

instance (QCD a, QCD b) => QueryDelete False (a, b) where
  queryDelete w = do
    deleteA <- queryDelete @(S a) @a w
    deleteB <- queryDelete @(S b) @b w
    pure $! \e -> deleteA e *> deleteB e
  {-# INLINE queryDelete #-}

instance (QCI a, QCI b) => QueryIterate False (a, b) where
  queryFor w = do
    membersA <- queryMembers @(S a) @a w
    membersB <- queryMembers @(S b) @b w
    forA <- queryFor @(S a) w
    forB <- queryFor @(S b) w
    containsA <- queryContains @(S a) @a w
    containsB <- queryContains @(S b) @b w
    getA <- queryGet @(S a) @a w
    getB <- queryGet @(S b) @b w
    pure $! \f -> do
      amountA <- membersA
      amountB <- membersB
      if amountA > amountB
        then
          forB $! \e bs -> do
            whenIO (containsA e) $! do
              as <- getA e
              f e (as, bs)
        else
          forA $! \e as -> do
            whenIO (containsA e) $! do
              bs <- getB e
              f e (as, bs)
  queryMembers w = do
    membersA <- queryMembers @(S a) @a w
    membersB <- queryMembers @(S b) @b w
    pure $! min <$!> membersA <*> membersB
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

instance (QCG a, QCG b, QCG c) => QueryGet False (a, b, c) where
  queryContains w = do
    containsA <- queryContains @(S a) @a w
    containsB <- queryContains @(S b) @b w
    containsC <- queryContains @(S c) @c w
    pure $! \e -> (\a b c -> a && b && c) <$!> containsA e <*> containsB e <*> containsC e
  queryGet w = do
    getA <- queryGet @(S a) w
    getB <- queryGet @(S b) w
    getC <- queryGet @(S c) w
    pure $! \e -> (,,) <$!> getA e <*> getB e <*> getC e
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}

instance (QCP a, QCP b, QCP c) => QueryPut False (a, b, c) where
  queryPut w = do
    putA <- queryPut @(S a) w
    putB <- queryPut @(S b) w
    putC <- queryPut @(S c) w
    pure $! \e (a, b, c) -> putA e a *> putB e b *> putC e c
  {-# INLINE queryPut #-}

instance (QCD a, QCD b, QCD c) => QueryDelete False (a, b, c) where
  queryDelete w = do
    deleteA <- queryDelete @(S a) @a w
    deleteB <- queryDelete @(S b) @b w
    deleteC <- queryDelete @(S c) @c w
    pure $! \e -> deleteA e *> deleteB e *> deleteC e
  {-# INLINE queryDelete #-}

instance (QCI a, QCI b, QCI c) => QueryIterate False (a, b, c) where
  queryFor w = do
    membersA <- queryMembers @(S a) @a w
    membersB <- queryMembers @(S b) @b w
    membersC <- queryMembers @(S c) @c w
    forA <- queryFor @(S a) w
    forB <- queryFor @(S b) w
    forC <- queryFor @(S c) w
    containsA <- queryContains @(S a) @a w
    containsB <- queryContains @(S b) @b w
    containsC <- queryContains @(S c) @c w
    getA <- queryGet @(S a) @a w
    getB <- queryGet @(S b) @b w
    getC <- queryGet @(S c) @c w
    pure $! \f -> do
      amountA <- membersA
      amountB <- membersB
      amountC <- membersC
      if amountA > amountB
        then
          if amountB > amountC
            then
              forC $! \e cs -> do
                whenIO (containsA e)
                  $! whenIO (containsB e)
                  $! do
                    as <- getA e
                    bs <- getB e
                    f e (as, bs, cs)
            else
              forB $! \e bs -> do
                whenIO (containsA e)
                  $! whenIO (containsC e)
                  $! do
                    as <- getA e
                    cs <- getC e
                    f e (as, bs, cs)
        else
          if amountA > amountC
            then
              forC $! \e cs -> do
                whenIO (containsA e)
                  $! whenIO (containsB e)
                  $! do
                    as <- getA e
                    bs <- getB e
                    f e (as, bs, cs)
            else
              forA $! \e as -> do
                whenIO (containsB e)
                  $! whenIO (containsC e)
                  $! do
                    cs <- getC e
                    bs <- getB e
                    f e (as, bs, cs)
  queryMembers w = do
    membersA <- queryMembers @(S a) @a w
    membersB <- queryMembers @(S b) @b w
    membersC <- queryMembers @(S c) @c w
    pure $! (\a b c -> min (min a b) c) <$!> membersA <*> membersB <*> membersC

  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

whenIO :: IO Bool -> IO () -> IO ()
whenIO action f = do
  b <- action
  when b f
{-# INLINE whenIO #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}