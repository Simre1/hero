{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Prelude hiding ((.), id)
import Data.Functor
import Data.Coerce
import Data.Monoid


data Query m i o where
  QueryPut :: QC a => Query m a ()
  QueryDelete :: QC a => Query m a ()
  QueryCompose :: Query m a b -> Query m b c -> Query m a c
  QueryMap :: (a -> m b) -> Query m a b
  QueryPar :: Query m i1 o1 -> Query m i2 o2 -> Query m (i1,i2) (o1,o2)

instance Applicative m => Category (Query m) where
  id = QueryMap pure
  (.) = flip QueryCompose
  {-# INLINE id #-}
  {-# INLINE (.) #-}


instance Applicative m => Arrow (Query m) where
  arr f = QueryMap (pure . f)
  (***) = QueryPar
  {-# INLINE (***) #-}
  {-# INLINE arr #-}


data System m i o where
  SystemExec :: QC i1 => Query m (i1, i2) () -> System m i2 ()
  SystemSingle :: Query m i o -> Entity -> System m i o
  SystemFor :: (QC i1, Monoid o) => Query m (i1,i2) o -> System m i2 o
  SystemNewEntity :: QC c => System m c Entity
  SystemMapM :: (i -> m o) -> System m i o
  SystemMap :: (i -> o) -> System m i o
  SystemSeq :: System m a b -> System m b c -> System m a c
  SystemPar :: System m a1 b1 -> System m a2 b2 -> System m (a1,a2) (b1,b2)

instance Applicative m => Functor (System m i) where
  fmap f !s = SystemSeq s (SystemMap $ f)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (System m i) where
  pure a = SystemMap $ \_ -> a
  !s1 <*> !s2 = SystemSeq (SystemSeq (SystemMap (\a -> (a,a))) 
    (SystemPar s1 s2)) (SystemMap (\(f,a) -> f a) )
  {-# INLINE (<*>) #-}
  {-# INLINE pure #-}



instance Applicative m => Category (System m) where
  id = SystemMap (id)
  !s1 . !s2 = SystemSeq s2 s1
  {-# INLINE id #-}
  {-# INLINE (.) #-}



instance Applicative m => Arrow (System m) where
  arr f = SystemMap f
  !s1 *** !s2 = SystemPar s1 s2
  {-# INLINE (***) #-}
  {-# INLINE arr #-}


-- newtype CompiledQuery m i a = CompiledQuery (i -> Entity -> m a)

data QueryContext = QueryContext ()

compileSystem :: World -> System IO i o -> IO (i -> IO o)
compileSystem !w q = case q of
  SystemExec @i1 !q -> do
    cQ <- compileQuery w q
    for <- queryFor @(S i1) @i1 w
    pure $ \i2 -> for $ \e i1 -> cQ (i1,i2) e
  SystemSingle !q !e -> do
    cQ <- compileQuery w q
    pure $ \i -> cQ i e
  SystemFor @i1 @o !q -> do
    cQ <- compileQuery w q
    for <- queryFor @(S i1) @i1 w
    pure $ \i2 -> do
      ref <- newIORef (mempty :: o)
      for $ \e i1 -> do
        o <- cQ (i1,i2) e
        modifyIORef ref (<> o)
      readIORef ref
  SystemNewEntity @c -> do
    put <- queryPut @(S c) @c w
    pure $ \c -> do
      e <- worldNewEntity w
      put e c
      pure e
  SystemMap !f -> pure $ pure . f
  SystemMapM !f -> pure f
  SystemSeq !s1 !s2 -> do
    f1 <- compileSystem w s1
    f2 <- compileSystem w s2
    pure $ \i -> f1 i >>= f2
  SystemPar !s1 !s2 -> do
    f1 <- compileSystem w s1
    f2 <- compileSystem w s2
    pure $ \(i1,i2) -> (,) <$> f1 i1 <*> f2 i2
{-# INLINE compileSystem #-}

compileQuery :: World -> Query IO i a -> IO (i -> Entity -> IO a)
compileQuery !w q = case q of
  QueryPut @p -> do
    put <- queryPut @(S p) @p w
    pure $ \i e -> liftIO $ put e i
  QueryDelete @d -> do
    delete <- queryDelete @(S d) @d w
    pure $ \i e -> liftIO $ delete e
  QueryCompose q1 q2 -> do
    cQ1 <- compileQuery w q1
    cQ2 <- compileQuery w q2
    pure $ \a e -> cQ1 a e >>= \b -> cQ2 b e
  QueryMap f -> pure $ \a e -> (f a)
  QueryPar q1 q2 -> do
    cQ1 <- compileQuery w q1
    cQ2 <- compileQuery w q2
    pure $ \(i1,i2) e -> (,) <$> cQ1 i1 e <*> cQ2 i2 e
{-# INLINE compileQuery #-}

cmap :: (QC a, QC b, Applicative m) => (a -> b) -> System m () ()
cmap f = SystemExec (arr (f . fst) >>> QueryPut)
{-# INLINE cmap #-}

cmapM :: (QC a, QC b) => (a -> IO b) -> System IO () ()
cmapM f = SystemExec (QueryMap (f . fst) >>> QueryPut)
{-# INLINE cmapM #-}

cfold :: (Monoid o, QC a, MonadIO m) => (a -> o) -> System m () o
cfold f = SystemFor (QueryMap (pure . f . fst))
{-# INLINE cfold #-}


cfoldM :: (Monoid o, QC a) => (a -> IO o) -> System IO i o
cfoldM f = SystemFor (QueryMap (f . fst))
{-# INLINE cfoldM #-}

cfoldr :: (QC a, MonadIO m) => (a -> b -> b) -> b -> System IO () b
cfoldr f b = fmap (($ b) . appEndo) $ cfold $ Endo #. f
{-# INLINE cfoldr #-}

cfoldl :: (QC a) => (b -> a -> b) -> b -> System IO () b
cfoldl f b = fmap (($ b) . appEndo . getDual) $ cfold $ Dual . Endo . flip f
{-# INLINE cfoldl #-}

-- newEntity' :: MonadIO m => System m Entity
-- newEntity' = askWorld >>= liftIO . worldNewEntity
-- {-# INLINE newEntity' #-}


-- newEntity :: (MPS a, MonadIO m) => a -> System m Entity
-- newEntity a = do
--   e <- newEntity' 
--   putEntity e a
--   pure e
-- {-# INLINE newEntity #-}


-- putEntity :: forall a m. (MonadIO m, MPS a) => Entity -> a -> System m ()
-- putEntity entity a = do
--   w <- askWorld
--   liftIO $ multiPut @(S a) w entity a
-- {-# INLINE putEntity #-}

-- runSystem :: World -> System m a -> m a
-- runSystem w (System r) = runReaderT r w
-- {-# INLINE runSystem #-}

-- -- See Data.Foldable
-- (#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
-- (#.) _f = coerce
-- {-# INLINE (#.) #-}
























type family S a where
  S () = False
  S (a, b) = False
  S (a, b, c) = False
  S _ = True

class QueryComponent (f :: Bool) components where
  queryContains :: World -> IO (Entity -> IO Bool)
  queryGet :: World -> IO (Entity -> IO components)
  queryPut :: World -> IO (Entity -> components -> IO ())
  queryDelete :: World -> IO (Entity -> IO ())
  queryFor :: World -> IO ((Entity -> components -> IO ()) -> IO ())
  queryMembers :: World -> IO (IO Int)

type QC (a :: *) = QueryComponent (S a) a

instance QueryComponent False () where
  queryContains w = pure $ \_ -> pure True
  queryGet w = pure $ \_ -> pure ()
  queryPut w = pure $ \_ _ -> pure ()
  queryDelete w = pure $ \_ -> pure ()
  queryFor w = do
    pure $ \f -> forEntities (worldEntities w) $ \e -> f e ()
  queryMembers w = do
    pure $ entityAmount (worldEntities w)
    

  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}
  {-# INLINE queryPut #-}
  {-# INLINE queryDelete #-}
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

instance Component a => QueryComponent True a where
  queryContains w = worldComponent @a w <&> \s e -> storeContains s e
  queryGet w = worldComponent @a w <&> \s e -> storeGet s e
  queryPut w = worldComponent w <&> \s e c -> storePut s e c
  queryDelete w = worldComponent @a w <&> \s e -> storeDelete s e
  queryFor w = worldComponent @a w <&> \s f -> storeFor s f
  queryMembers w = worldComponent @a w <&> storeMembers
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}
  {-# INLINE queryPut #-}
  {-# INLINE queryDelete #-}
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

instance (QC a, QC b) => QueryComponent False (a, b) where
  queryContains w = (\p1 p2 e -> (&&) <$> p1 e <*> p2 e) <$> queryContains @(S a) @a w <*> queryContains @(S b) @b w
  queryGet w = (\f1 f2 e -> (,) <$> f1 e <*> f2 e) <$> queryGet @(S a) @a w <*> queryGet @(S b) @b w
  queryPut w = do
    putA <- queryPut @(S a) w
    putB <- queryPut @(S b) w
    pure $ \e (a,b) -> putA e a *> putB e b
  queryDelete w = do
    deleteA <- queryDelete @(S a) @a w
    deleteB <- queryDelete @(S b) @b w
    pure $ \e -> deleteA e *> deleteB e
  queryFor w = do
    membersA <- queryMembers @(S a) @a w
    membersB <- queryMembers @(S b) @b w
    forA <- queryFor @(S a) w
    forB <- queryFor @(S b) w
    containsA <- queryContains @(S a) @a w
    containsB <- queryContains @(S b) @b w
    getA <- queryGet @(S a) @a w
    getB <- queryGet @(S b) @b w
    pure $ \f -> do
      amountA <- membersA
      amountB <- membersB
      if amountA > amountB
        then forB $ \e bs -> do
          whenIO (containsA e) $ do
            as <- getA e
            f e (as, bs)
        else forA $ \e as -> do
          whenIO (containsA e) $ do
            bs <- getB e
            f e (as, bs)
  queryMembers w = do
    membersA <- queryMembers @(S a) @a w
    membersB <- queryMembers @(S b) @b w
    pure $ min <$> membersA <*> membersB
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}
  {-# INLINE queryPut #-}
  {-# INLINE queryDelete #-}
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}


instance (QC a, QC b, QC c) => QueryComponent False (a, b, c) where
  queryContains w = do
    containsA <- queryContains @(S a) @a w
    containsB <- queryContains @(S b) @b w
    containsC <- queryContains @(S c) @c w
    pure $ \e -> (\a b c -> a && b && c) <$> containsA e <*> containsB e <*> containsC e
  queryGet w = do
    getA <- queryGet @(S a) w
    getB <- queryGet @(S b) w
    getC <- queryGet @(S c) w
    pure $ \e -> (,,) <$> getA e <*> getB e <*> getC e
  queryPut w = do
    putA <- queryPut @(S a) w
    putB <- queryPut @(S b) w
    putC <- queryPut @(S c) w
    pure $ \e (a,b,c) -> putA e a *> putB e b *> putC e c
  queryDelete w = do
    deleteA <- queryDelete @(S a) @a w
    deleteB <- queryDelete @(S b) @b w
    deleteC <- queryDelete @(S c) @c w
    pure $ \e -> deleteA e *> deleteB e *> deleteC e
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
    pure $ \f -> do
      amountA <- membersA
      amountB <- membersB
      amountC <- membersC
      if amountA > amountB
      then
        if amountB > amountC
          then forC $ \e cs -> do
            whenIO (containsA e) $
              whenIO (containsB e) $ do
                as <- getA e
                bs <- getB e
                f e (as, bs, cs)
          else forB $ \e bs -> do
            whenIO (containsA e) $
              whenIO (containsC e) $ do
                as <- getA e
                cs <- getC e
                f e (as, bs, cs)
      else
        if amountA > amountC
          then forC $ \e cs -> do
            whenIO (containsA e) $
              whenIO (containsB e) $ do
                as <- getA e
                bs <- getB e
                f e (as, bs, cs)
          else forA $ \e as -> do
            whenIO (containsB e) $
              whenIO (containsC e) $ do
                cs <- getC e
                bs <- getB e
                f e (as, bs, cs)
  queryMembers w = do
    membersA <- queryMembers @(S a) @a w
    membersB <- queryMembers @(S b) @b w
    membersC <- queryMembers @(S c) @c w
    pure $ (\a b c -> min (min a b) c) <$> membersA <*> membersB <*> membersC
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}
  {-# INLINE queryPut #-}
  {-# INLINE queryDelete #-}
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

-- instance (QC a, QC b, QC c) => QueryComponent False (a, b, c) where
--   queryContains w e = (&&) <$> ((&&) <$> queryContains @(S a) @a w e <*> queryContains @(S b) @b w e) <*> queryContains @(S c) @c w e
--   queryGet w e = (,,) <$> queryGet @(S a) @a w e <*> queryGet @(S b) @b w e <*> queryGet @(S c) @c w e
--   queryPut w e (a, b, c) = queryPut @(S a) w e a *> queryPut @(S b) w e b *> queryPut @(S c) @c w e c
--   queryDelete w e = queryDelete @(S a) @a w e *> queryDelete @(S b) @b w e *> queryDelete @(S c) @c w e
--   queryFor w f = do
--     amountA <- queryMembers @(S a) @a w
--     amountB <- queryMembers @(S b) @b w
--     amountC <- queryMembers @(S c) @c w
--     if amountA > amountB
--       then
--         if amountB > amountC
--           then queryFor @(S c) @c w $ \e cs -> do
--             whenIO (queryContains @(S a) @a w e) $
--               whenIO (queryContains @(S b) @b w e) $ do
--                 as <- queryGet @(S a) @a w e
--                 bs <- queryGet @(S b) @b w e
--                 f e (as, bs, cs)
--           else queryFor @(S b) @b w $ \e bs -> do
--             whenIO (queryContains @(S a) @a w e) $
--               whenIO (queryContains @(S c) @c w e) $ do
--                 as <- queryGet @(S a) @a w e
--                 cs <- queryGet @(S c) @c w e
--                 f e (as, bs, cs)
--       else
--         if amountA > amountC
--           then queryFor @(S c) @c w $ \e cs -> do
--             whenIO (queryContains @(S a) @a w e) $
--               whenIO (queryContains @(S b) @b w e) $ do
--                 as <- queryGet @(S a) @a w e
--                 bs <- queryGet @(S b) @b w e
--                 f e (as, bs, cs)
--           else queryFor @(S a) @a w $ \e as -> do
--             whenIO (queryContains @(S b) @b w e) $
--               whenIO (queryContains @(S c) @c w e) $ do
--                 cs <- queryGet @(S c) @c w e
--                 bs <- queryGet @(S b) @b w e
--                 f e (as, bs, cs)
--   queryMembers w = min <$> (min <$> queryMembers @(S a) @a w <*> queryMembers @(S b) @b w) <*> queryMembers @(S c) @c w
--   {-# INLINE queryContains #-}
--   {-# INLINE queryGet #-}
--   {-# INLINE queryPut #-}
--   {-# INLINE queryDelete #-}
--   {-# INLINE queryMembers #-}

whenIO :: IO Bool -> IO () -> IO ()
whenIO action f = do
  b <- action
  when b f

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}