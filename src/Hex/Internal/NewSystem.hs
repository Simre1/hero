{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Internal.NewSystem where

import Control.Arrow
import Control.Category
import Control.Monad (when, (<$!>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.Functor
import Data.IORef
import Data.Monoid
import Hex.Internal.Component
import Hex.Internal.Entity
import Hex.Internal.World
import Language.Haskell.TH
import Prelude hiding (id, (.))

newtype Query m i o = Query (World -> IO (Entity -> i -> IO o))

instance Applicative m => Category (Query m) where
  id = Query $ \_ -> pure $ \_ i -> pure i
  (Query makeQ1) . (Query makeQ2) = Query $ \w -> do
    q1 <- makeQ1 w
    q2 <- makeQ2 w
    pure $ \e i -> q2 e i >>= q1 e

instance Applicative m => Arrow (Query m) where
  arr f = Query $ \_ -> pure $ \_ -> pure . f
  (Query makeQ1) *** (Query makeQ2) = Query $ \w -> do
    q1 <- makeQ1 w
    q2 <- makeQ2 w
    pure $ \e (i1, i2) -> (,) <$> q1 e i1 <*> q2 e i2

qput :: forall i m. QC i => Query IO i ()
qput = Query $ \w -> do
  put <- queryPut @(S i) @i w
  pure put

qdelete :: forall i m. QC i => Query IO i ()
qdelete = Query $ \w -> do
  delete <- queryDelete @(S i) @i w
  pure $! \e _ -> delete e

newtype System m i o = System (World -> IO (i -> IO o))

instance Applicative m => Category (System m) where
  id = System (\_ -> pure (pure . id))
  (System f1) . (System f2) = System $ \w -> do
    f1' <- f1 w
    f2' <- f2 w
    pure $ \i -> f2' i >>= f1'
  {-# INLINE id #-}
  {-# INLINE (.) #-}


instance Applicative m => Arrow (System m) where
  arr f = System (\_ -> pure (pure . f))
  (System f1) *** (System f2) = System $ \w -> do
    f1' <- f1 w
    f2' <- f2 w
    pure $ \(i1, i2) -> (,) <$> f1' i1 <*> f2' i2
  {-# INLINE arr #-}
  {-# INLINE (***) #-}


instance Applicative m => Functor (System m i) where
  fmap f (System makeQ) = System $ \w -> do
    q <- makeQ w
    pure $ \i -> fmap f (q i)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (System m i) where
  pure a = System $ \_ -> pure $ \_ -> pure a
  (System makeQF) <*> (System makeQV) = System $ \w -> do
    f <- makeQF w
    v <- makeQV w
    pure $ \i -> f i <*> v i
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}


compileSystem :: System IO i o -> World -> IO (i -> IO o)
compileSystem (System f) w = f w

cmap :: forall a b m. (QC a, QC b) => (a -> b) -> System IO () ()
cmap f =
  System
    ( \w -> do
        for <- queryFor @(S a) @a w
        put <- queryPut @(S b) @b w
        pure $ \_ -> for $ \e a -> put e (f a)
    )

cmapM :: forall a b m. (QC a, QC b) => (a -> IO b) -> System IO () ()
cmapM f =
  System
    ( \w -> do
        for <- queryFor @(S a) @a w
        put <- queryPut @(S b) @b w
        pure $ \_ -> for $ \e a -> f a >>= put e
    )

cfold :: forall a o m. (Monoid o, QC a) => (a -> o) -> System IO () o
cfold f = System $ \w -> do
  for <- queryFor @(S a) @a w
  pure $! \i2 -> do
    ref <- newIORef (mempty :: o)
    for $! \e a -> do
      let o = f a
      modifyIORef ref (<> o)
    readIORef ref

cfoldM :: forall a o m. (Monoid o, QC a) => (a -> IO o) -> System IO () o
cfoldM f = System $ \w -> do
  for <- queryFor @(S a) @a w
  pure $! \i2 -> do
    ref <- newIORef (mempty :: o)
    for $! \e a -> do
      o <- f a
      modifyIORef ref (<> o)
    readIORef ref

cfoldr :: (QC a) => (a -> b -> b) -> b -> System IO () b
cfoldr f b = fmap (($! b) . appEndo) $! cfold $! Endo #. f
{-# INLINE cfoldr #-}

cfoldl :: (QC a) => (b -> a -> b) -> b -> System IO () b
cfoldl f b = fmap (($! b) . appEndo . getDual) $! cfold $! Dual . Endo . flip f
{-# INLINE cfoldl #-}

newEntity :: forall a. (QC a) => System IO a Entity
newEntity = System $ \w -> do
  put <- queryPut @(S a) @a w
  pure $! \c -> do
    e <- worldNewEntity w
    put e c
    pure e


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
  queryContains w = pure $! \_ -> pure True
  queryGet w = pure $! \_ -> pure ()
  queryPut w = pure $! \_ _ -> pure ()
  queryDelete w = pure $! \_ -> pure ()
  queryFor w = do
    pure $! \f -> forEntities (worldEntities w) $! \e -> f e ()
  queryMembers w = do
    pure $! entityAmount (worldEntities w)

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
  queryContains w = (\p1 p2 e -> (&&) <$!> p1 e <*> p2 e) <$!> queryContains @(S a) @a w <*> queryContains @(S b) @b w
  queryGet w = (\f1 f2 e -> (,) <$!> f1 e <*> f2 e) <$!> queryGet @(S a) @a w <*> queryGet @(S b) @b w
  queryPut w = do
    putA <- queryPut @(S a) w
    putB <- queryPut @(S b) w
    pure $! \e (a, b) -> putA e a *> putB e b
  queryDelete w = do
    deleteA <- queryDelete @(S a) @a w
    deleteB <- queryDelete @(S b) @b w
    pure $! \e -> deleteA e *> deleteB e
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
    pure $! \e -> (\a b c -> a && b && c) <$!> containsA e <*> containsB e <*> containsC e
  queryGet w = do
    getA <- queryGet @(S a) w
    getB <- queryGet @(S b) w
    getC <- queryGet @(S c) w
    pure $! \e -> (,,) <$!> getA e <*> getB e <*> getC e
  queryPut w = do
    putA <- queryPut @(S a) w
    putB <- queryPut @(S b) w
    putC <- queryPut @(S c) w
    pure $! \e (a, b, c) -> putA e a *> putB e b *> putC e c
  queryDelete w = do
    deleteA <- queryDelete @(S a) @a w
    deleteB <- queryDelete @(S b) @b w
    deleteC <- queryDelete @(S c) @c w
    pure $! \e -> deleteA e *> deleteB e *> deleteC e
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
  {-# INLINE queryContains #-}
  {-# INLINE queryGet #-}
  {-# INLINE queryPut #-}
  {-# INLINE queryDelete #-}
  {-# INLINE queryFor #-}
  {-# INLINE queryMembers #-}

-- instance (QC a, QC b, QC c) => QueryComponent False (a, b, c) where
--   queryContains w e = (&&) <$!> ((&&) <$!> queryContains @(S a) @a w e <*> queryContains @(S b) @b w e) <*> queryContains @(S c) @c w e
--   queryGet w e = (,,) <$!> queryGet @(S a) @a w e <*> queryGet @(S b) @b w e <*> queryGet @(S c) @c w e
--   queryPut w e (a, b, c) = queryPut @(S a) w e a *> queryPut @(S b) w e b *> queryPut @(S c) @c w e c
--   queryDelete w e = queryDelete @(S a) @a w e *> queryDelete @(S b) @b w e *> queryDelete @(S c) @c w e
--   queryFor w f = do
--     amountA <- queryMembers @(S a) @a w
--     amountB <- queryMembers @(S b) @b w
--     amountC <- queryMembers @(S c) @c w
--     if amountA > amountB
--       then
--         if amountB > amountC
--           then queryFor @(S c) @c w $! \e cs -> do
--             whenIO (queryContains @(S a) @a w e) $!
--               whenIO (queryContains @(S b) @b w e) $! do
--                 as <- queryGet @(S a) @a w e
--                 bs <- queryGet @(S b) @b w e
--                 f e (as, bs, cs)
--           else queryFor @(S b) @b w $! \e bs -> do
--             whenIO (queryContains @(S a) @a w e) $!
--               whenIO (queryContains @(S c) @c w e) $! do
--                 as <- queryGet @(S a) @a w e
--                 cs <- queryGet @(S c) @c w e
--                 f e (as, bs, cs)
--       else
--         if amountA > amountC
--           then queryFor @(S c) @c w $! \e cs -> do
--             whenIO (queryContains @(S a) @a w e) $!
--               whenIO (queryContains @(S b) @b w e) $! do
--                 as <- queryGet @(S a) @a w e
--                 bs <- queryGet @(S b) @b w e
--                 f e (as, bs, cs)
--           else queryFor @(S a) @a w $! \e as -> do
--             whenIO (queryContains @(S b) @b w e) $!
--               whenIO (queryContains @(S c) @c w e) $! do
--                 cs <- queryGet @(S c) @c w e
--                 bs <- queryGet @(S b) @b w e
--                 f e (as, bs, cs)
--   queryMembers w = min <$!> (min <$!> queryMembers @(S a) @a w <*> queryMembers @(S b) @b w) <*> queryMembers @(S c) @c w
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