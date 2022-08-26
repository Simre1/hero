{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Hero.System.System where

import Control.Arrow (Arrow (arr, (&&&), (***)))
import Control.Category (Category (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Vector qualified as V
import Hero.Component.Component (ComponentId)
import Hero.Parallel.ExecutionPlanner
import Hero.World (World)
import Prelude hiding (id, (.))


data Dependency = DependComponent ComponentId | DependEntity | DependAll

-- | A system is a function which can operate on the components of a world.
-- Keep in mind that system has Functor, Applicative, Category and Arrow instances, but no Monad instance.
newtype System (i :: Type) (o :: Type) = System (World -> IO (ExecutionPlan Dependency i o))

instance Category System where
  id = System (\_ -> pure (Action noResources (pure . id)))
  (System f1) . (System f2) = System $ \w -> do
    f2' <- f2 w
    f1' <- f1 w
    pure $ Sequence f2' f1'
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow System where
  arr f = System (\_ -> pure (Action noResources $ pure . f))
  (System f1) *** (System f2) = System $ \w -> do
    f1' <- f1 w
    f2' <- f2 w
    pure $ Parallel (Map f1' $ \f1'' (i1,i2) -> (,) <$> f1'' i1) (Map f2' $ \f2'' (i1,i2) -> f2'' i2)
  {-# INLINE arr #-}
  {-# INLINE (***) #-}

instance Functor (System i) where
  fmap f (System makeS) = System $ \w -> do
    s <- makeS w
    pure $ Map s $ \s' -> \i -> fmap f (s' i)
  {-# INLINE fmap #-}

instance Applicative (System i) where
  pure a = System $ \_ -> pure $ Action noResources $ \_ -> pure a

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
withSetup' f = System $ \w -> f w >>= \a -> pure $ Action noResources (\_ -> pure a)

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
liftSystem f = System $ \w -> pure $ Action noResources f
{-# INLINE liftSystem #-}

-- | Compiles a system with the given World and returns a function which executes
-- the operations within the System.
compileSystem :: System i o -> World -> IO (i -> IO o)
compileSystem (System f) w = f w >>= compileExecutionPlan


-- | Forwards the input of a system to its output. The previous output is ignored.
-- Look at (&&&) if you do not want to ignore the output.
forward :: System i o -> System i i
forward (System makeS) = System $ \w -> do
  s <- makeS w
  pure $ Map s (\s' i -> i <$ s' i)
{-# INLINE forward #-}

-- | Executes a system only once and caches the output, then returns that output
-- continously.
once :: System i o -> System i o
once (System makeS) = System $ \w -> do
  s' <- makeS w
  pure $
    WithCompiled [s'] $ \[s] -> do
      ref <- newIORef (\_ -> pure undefined)
      writeIORef ref $ \i -> s i >>= \o -> liftIO (writeIORef ref (\_ -> pure o)) >> pure o
      pure $ Action noResources $ \i -> liftIO (readIORef ref) >>= \f -> f i
{-# INLINE once #-}

-- | Feed back the output value as input in in the next iteration.
feedback :: s -> System (i, s) (o, s) -> System i o
feedback s (System makeS) = System $ \w -> do
  plan <- makeS w
  ref <- liftIO $ newIORef s
  pure $
    Map plan $ \runS i -> do
      s' <- liftIO $ readIORef ref
      (o, s'') <- runS (i, s')
      liftIO $ writeIORef ref s''
      pure o
{-# INLINE feedback #-}

-- | Depending on the output of the last system, runs one or the other system, similar to the bool function.
-- If output == False, then the first system is run (false -> left).
-- If output == True, then the second system is run (true -> right).
-- Compiles all three systems.
ifS :: System i o -> System i o -> System i Bool -> System i o
ifS (System makeS1) (System makeS2) decider = System $ \w -> do
  let (System makeSD) = id &&& decider
  sd <- makeSD w
  s1 <- makeS1 w
  s2 <- makeS2 w
  pure $
    Sequence sd $
      WithCompiled [s1, s2] $ \[s1', s2'] -> pure $
        Action noResources $ \(i, whichSystem) -> do
          if whichSystem
            then s2' i
            else s1' i

-- | Depending on the output of the last system, runs the system created by the given function.
-- Runs the system which the function matches to the `enum`.
-- Compiles systems for all `Enum` values. Do not use this for large `Enum`s!!
ifEnum :: (Enum enum) => (enum -> System i o) -> System i enum -> System i o
ifEnum makeSystem decider = System $ \w -> do
  let (System makeSD) = id &&& decider
  sd <- makeSD w

  plans <- sequenceA $ (($ w) . coerce . makeSystem) <$> enums

  pure $
    Sequence sd $
      WithCompiled plans $ \plans' -> do
        let compiledSystems = V.fromList plans'
        pure $ Action noResources $ \(i, whichSystem) -> (compiledSystems V.! (fromEnum whichSystem)) i
  where
    enums = enumFrom (toEnum 0)