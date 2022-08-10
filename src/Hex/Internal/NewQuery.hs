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
import Prelude hiding ((.))
import Data.Functor


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

type QC a = QueryComponent (S a) a

-- type S a = SingleComponent a

-- instance QueryComponent False () where
--   queryContains w = pure $ \_ -> pure True
--   queryGet w = pure $ \_ -> pure ()
--   queryPut w = pure $ \_ _ -> pure ()
--   queryDelete w = pure $ \_ -> pure ()
--   queryFor w = pure $ \_ -> pure ()
--   queryMembers w = pure $ 
--   {-# INLINE queryContains #-}
--   {-# INLINE queryGet #-}
--   {-# INLINE queryPut #-}
--   {-# INLINE queryDelete #-}
--   {-# INLINE queryFor #-}
--   {-# INLINE queryMembers #-}

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