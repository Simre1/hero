{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Hero.System.System where

import Control.Arrow (Arrow (arr, (&&&), (***)))
import Control.Category (Category (..))
import Control.Concurrent.Async (Concurrently (runConcurrently))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Vector qualified as V
import Hero.Component.Component (ComponentId)
import Hero.System.ExecutionPlanner
import Hero.World (World)
import Prelude hiding (id, (.))

-- | A system is a function which can operate on the components of a world.
-- Keep in mind that system has Functor, Applicative, Category and Arrow instances, but no Monad instance.
newtype System (i :: Type) (o :: Type) = System (World -> IO (ExecutionPlan i o))

instance Category System where
  id = System (\_ -> pure (Action mempty (pure . id)))
  (System f1) . (System f2) = System $ \w -> do
    f2' <- f2 w
    f1' <- f1 w
    pure $ Sequence f2' f1'
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow System where
  arr f = System (\_ -> pure (Action mempty $ pure . f))
  (System f1) *** (System f2) = System $ \w -> do
    f1' <- f1 w
    f2' <- f2 w
    pure $ Parallel (MapOutput (MapInput f1' fst) (,)) (MapInput f2' snd)
  {-# INLINE arr #-}
  {-# INLINE (***) #-}

instance Functor (System i) where
  fmap f (System makeS) = System $ \w -> do
    s <- makeS w
    pure $ MapOutput s f
  {-# INLINE fmap #-}

instance Applicative (System i) where
  pure a = System $ \_ -> pure $ Action mempty $ \_ -> pure a

  -- (<*>) might be automatically parallelized in the future. Use (>>>) for sequential code.
  (System makeSF) <*> (System makeSV) = System $ \w -> do
    f <- makeSF w
    v <- makeSV w
    pure $ Parallel f v
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Allows code to be executed during the compilation stage.
-- `IO a` is only executed once and the `a` is the permanent output of the system.
withSetup' :: (World -> IO a) -> System i a
withSetup' f = System $ \w -> f w >>= \a -> pure $ Action mempty (\_ -> pure a)

-- | Allows code to be executed during the compilation stage.
-- `IO a` is only executed once and the `a` is the permanent output of the system.
withSetup :: (World -> IO a) -> (a -> System i o) -> System i o
withSetup f make = System $ \w -> do
  a <- f w
  let System makeS = make a
  s <- makeS w
  pure s

-- | Lifs a normal function into a System.
liftSystem :: (a -> IO b) -> System a b
liftSystem f = System $ \w -> pure $ Action mempty f
{-# INLINE liftSystem #-}

-- | Compiles a system with the given World and returns a function which executes
-- the operations within the System.
compileSystem :: System i o -> World -> IO (i -> IO o)
compileSystem (System f) w = f w >>= compileExecutionPlan

compilePlan :: System i o -> World -> IO (FlatExecutionPlan i o)
compilePlan (System f) w = snd . flattenPlan <$> f w

-- | Forwards the input of a system to its output. The previous output is ignored.
-- Look at (&&&) if you do not want to ignore the output.
forward :: System i o -> System i i
forward s = arr snd . (s &&& id)
{-# INLINE forward #-}

-- | Executes a system only once and caches the output, then returns that output
-- continously.
once :: System i o -> System i o
once (System makeS) = System $ \w -> do
  s' <- makeS w
  pure $ Once s'
{-# INLINE once #-}

-- | Feed back the output value as input in in the next iteration.
-- feedback :: s -> System (i, s) (o, s) -> System i o
-- feedback s (System makeS) = System $ \w -> do
--   plan <- makeS w
--   ref <- liftIO $ newIORef s
--   pure $
--     Map plan $ \runS i -> do
--       s' <- readIORef ref
--       (o, s'') <- runS (i, s')
--       liftIO $ writeIORef ref s''
--       pure o
-- {-# INLINE feedback #-}

-- | Depending on the output of the last system, runs one or the other system, similar to the bool function.
-- If output == False, then the first system is run (false -> left).
-- If output == True, then the second system is run (true -> right).
-- Compiles all three systems.
ifS :: System i o -> System i o -> System i Bool -> System i o
ifS sFalse sTrue decider = ifEnum (\b -> if b then sTrue else sFalse) decider

-- | Depending on the output of the last system, runs the system created by the given function.
-- Runs the system which the function matches to the `enum`.
-- Compiles systems for all `Enum` values. Do not use this for large `Enum`s!!
ifEnum :: (Enum enum) => (enum -> System i o) -> System i enum -> System i o
ifEnum makeSystem (System makeSD) = System $ \w -> do
  sd <- makeSD w
  plans <- sequenceA $ (($ w) . coerce . makeSystem) <$> enums
  pure $ Decide plans (MapOutput sd fromEnum)
  where
    enums = enumFrom (toEnum 0)