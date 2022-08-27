{-# LANGUAGE LambdaCase #-}

module Hero.System.ExecutionPlanner where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Concurrent.Async
import Data.Functor.Identity
import Data.Graph qualified as G
import Data.IORef
import Data.IntMap qualified as IM
import Data.Set
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Void
import GHC.Generics (Generic)
import Hero.Component.Component (ComponentId (..))
import Type.Reflection
import Prelude hiding (id, (.))

data Resource = ResComponent ComponentId | ResEntity | ResAll deriving (Eq, Ord, Show)

data Dependencies = Dependencies
  { read :: S.Set Resource,
    write :: S.Set Resource
  }
  deriving (Generic, Ord, Eq)

instance Show (Dependencies) where
  show (Dependencies r w) = show (S.toList r, S.toList w)

instance Semigroup (Dependencies) where
  (Dependencies r1 w1) <> (Dependencies r2 w2) = Dependencies (r1 <> r2) (w1 <> w2)

instance Monoid (Dependencies) where
  mempty = Dependencies S.empty S.empty

makeDependencies :: [Resource] -> [Resource] -> Dependencies
makeDependencies r w = Dependencies (S.fromList r) (S.fromList w)

data Combine f a where
  CombineApp :: Combine f (a -> b) -> Combine f a -> Combine f b
  CombineAction :: f a -> Combine f a

-- data Sequence f a where
--   SequenceThen :: f a -> (a -> Sequence f b) -> Sequence f b
--   SequencePure :: f a -> Sequence f a

-- data ExecutionPlan r m a b where
--   Sequence :: ExecutionPlan r m a x -> ExecutionPlan r m x b -> ExecutionPlan r m a b
--   Parallel :: ExecutionPlan r m a (x -> b) -> ExecutionPlan r m a x -> ExecutionPlan r m a b
--   Execute :: Dependency r -> (a -> m b) -> ExecutionPlan r m a b
--   MapInput :: (a -> a') -> ExecutionPlan r m a' b -> ExecutionPlan r m a b
--   MapOutput :: (b' -> b) -> ExecutionPlan r m a b' -> ExecutionPlan r m a b
--   Forward :: ExecutionPlan r m a b -> ExecutionPlan r m a a

data ExecutionPlan a b where
  Parallel :: ExecutionPlan i (a -> b) -> ExecutionPlan i a -> ExecutionPlan i b
  Sequence :: ExecutionPlan a b -> ExecutionPlan b c -> ExecutionPlan a c
  MapInput :: ExecutionPlan a' b -> (a -> a') -> ExecutionPlan a b
  MapOutput :: ExecutionPlan a b' -> (b' -> b) -> ExecutionPlan a b
  Action :: Dependencies -> (a -> IO b) -> ExecutionPlan a b
  Decide :: [ExecutionPlan a b] -> ExecutionPlan a Int -> ExecutionPlan a b
  Once :: ExecutionPlan a b -> ExecutionPlan a b

data FlatExecutionPlan a b where
  FlatParallel :: Combine (FlatExecutionPlan a) b -> FlatExecutionPlan a b
  FlatSequence :: FlatExecutionPlan a b -> FlatExecutionPlan b c -> FlatExecutionPlan a c
  -- FlatParallelAction :: Dependencies -> f a b -> FlatExecutionPlan a b
  FlatAction :: (a -> IO b) -> FlatExecutionPlan a b
  FlatDecide :: [FlatExecutionPlan a b] -> FlatExecutionPlan a Int -> FlatExecutionPlan a b
  FlatOnce :: FlatExecutionPlan a b -> FlatExecutionPlan a b

-- FlatMainThread :: Dependencies -> (a -> IO b) -> FlatExecutionPlan a b
-- FlatMap :: FlatExecutionPlan a' b' -> ((a' -> IO b') -> a -> IO b) -> FlatExecutionPlan a b
-- FlatWithCompiled :: [FlatExecutionPlan a' b'] -> ([a' -> IO b'] -> IO (FlatExecutionPlan a b)) -> FlatExecutionPlan a b

instance Show (Combine (FlatExecutionPlan a) b) where
  show (CombineAction a) = show a
  show (CombineApp f a) = show f <> " <*> " <> show a

instance Show (FlatExecutionPlan a b) where
  show (FlatParallel c) = "(" <> show c <> ")"
  show (FlatAction c) = "Action"
  show (FlatSequence a b) = "(" <> show a <> " >> " <> show b <> ")"

data Test a b = Test deriving (Show)

test :: ExecutionPlan Double Double
test =
  Sequence
    ( Parallel
        ( Parallel
            (Action mempty undefined)
            (Action mempty undefined)
        )
        (Action mempty undefined)
    )
    (Parallel (Action mempty undefined) $ Parallel (Action mempty undefined) (Action mempty undefined))

test2 :: ExecutionPlan Double Double
test2 = Parallel (Action mempty undefined) (Sequence (Action mempty undefined) (Action mempty undefined))

flattenPlan :: ExecutionPlan a b -> (Dependencies, FlatExecutionPlan a b)
flattenPlan = \case
  Parallel a b ->
    let (depsA, a') = flattenPlan a
        (depsB, b') = flattenPlan b
     in (depsA <> depsB,) $
          if parallizable depsA depsB
            then case a' of
              FlatParallel a' -> case b' of
                FlatParallel b' -> FlatParallel $ CombineApp a' b'
                action -> FlatParallel $ CombineApp a' (CombineAction action)
              actionA -> case b' of
                FlatParallel b'' -> FlatParallel $ CombineApp (CombineAction actionA) b''
                actionB -> FlatParallel $ CombineApp (CombineAction actionA) (CombineAction actionB)
            else
              let b'' =
                    FlatParallel $
                      CombineApp
                        (CombineAction (FlatAction (\i -> pure (i,))))
                        (CombineAction b')
                  a'' =
                    FlatParallel $
                      CombineApp
                        (CombineAction $ mapInput fst a')
                        (CombineAction (FlatAction (pure . snd)))
               in FlatSequence
                    b''
                    a''
  Sequence a b ->
    let (depsA, a') = flattenPlan a
        (depsB, b') = flattenPlan b
     in (depsA <> depsB, FlatSequence a' b')
  MapInput a f -> second (mapInput f) (flattenPlan a)
  MapOutput a f -> second (mapOutput f) (flattenPlan a)
  Action deps a -> (deps, FlatAction a)
  Decide plans decider ->
    let (depsP, p') = unzip $ flattenPlan <$> plans
        (depsD, d') = flattenPlan decider
     in (depsD <> mconcat depsP, FlatDecide p' d')
  Once p -> second FlatOnce (flattenPlan p)
  where
    mapOutput :: (b' -> b) -> FlatExecutionPlan a b' -> FlatExecutionPlan a b
    mapOutput f (FlatAction a) = FlatAction (fmap f . a)
    mapOutput f (FlatSequence a1 a2) = FlatSequence a1 (mapOutput f a2)
    mapOutput f (FlatParallel c) = FlatParallel (mapOutputCombine f c)
    mapOutput f (FlatDecide plans d) = FlatDecide (mapOutput f <$> plans) d
    mapOutput f (FlatOnce p) = FlatOnce (mapOutput f p)
    mapOutputCombine :: (b' -> b) -> Combine (FlatExecutionPlan a) b' -> Combine (FlatExecutionPlan a) b
    mapOutputCombine f (CombineApp a1 a2) = CombineApp (mapOutputCombine (f .) a1) a2
    mapOutputCombine f (CombineAction p) = CombineAction $ mapOutput f p
    mapInput :: (a -> a') -> FlatExecutionPlan a' b -> FlatExecutionPlan a b
    mapInput f (FlatAction a) = FlatAction (a . f)
    mapInput f (FlatSequence a1 a2) = FlatSequence (mapInput f a1) a2
    mapInput f (FlatParallel c) = FlatParallel $ mapInputCombine f c
    mapInput f (FlatDecide plans d) = FlatDecide (mapInput f <$> plans) (mapInput f d)
    mapInput f (FlatOnce p) = FlatOnce (mapInput f p)
    mapInputCombine :: (a -> a') -> Combine (FlatExecutionPlan a') b -> Combine (FlatExecutionPlan a) b
    mapInputCombine f (CombineAction a) = CombineAction (mapInput f a)
    mapInputCombine f (CombineApp a1 a2) = CombineApp (mapInputCombine f a1) (mapInputCombine f a2)

parallizable :: Dependencies -> Dependencies -> Bool
parallizable d1 d2 = noSharedComponents d1 d2 && noGlobalLock d1 d2 && noGlobalLock d2 d1
  where
    noGlobalLock (Dependencies r1 w1) (Dependencies r2 w2) =
      not $
        S.member ResAll w1 && (S.null r2 && S.null w2) || S.member ResAll r1 && S.null w2
    noSharedComponents (Dependencies r1 w1) (Dependencies r2 w2) =
      all
        id
        [S.disjoint w1 r2, S.disjoint w1 w2, S.disjoint w2 r1, S.disjoint w2 w1]

compileExecutionPlan :: ExecutionPlan a b -> IO (a -> IO b)
compileExecutionPlan plan = compileFlatPlan $ snd (flattenPlan plan)

compileFlatPlan :: FlatExecutionPlan a b -> IO (a -> IO b)
compileFlatPlan = \case
  FlatParallel c -> compileCombine c
  FlatSequence a b -> do
    a' <- compileFlatPlan a
    b' <- compileFlatPlan b
    pure $ \i -> a' i >>= b'
  FlatAction c -> pure c
  FlatOnce p -> do
    ref <- newIORef undefined
    action <- compileFlatPlan p
    writeIORef ref $ \i ->
      action i
        >>= \v -> writeIORef ref (\_ -> pure v) >> pure v
    pure $ \i -> readIORef ref >>= \action -> action i
  FlatDecide plans decider -> do
    plans' <- traverse compileFlatPlan plans
    let planVec = V.fromList plans'
    decide <- compileFlatPlan decider
    pure $ \i -> decide i >>= \n -> (planVec V.! n) i

compileCombine :: Combine (FlatExecutionPlan a) b -> IO (a -> IO b)
compileCombine (CombineAction a) = compileFlatPlan a
compileCombine (CombineApp f a) = do
  f' <- compileCombine f
  a' <- compileCombine a
  pure $ \i -> f' i <*> a' i

-- ionPlanner r = ExecutionPlanner
--   { anyThread :: Unagi.InChan (IO a)
--   }

-- runParallal :: Combine IO a -> IO a
-- runParallel

-- data ScheduledTree f a = Parallel [Combine ScheduledTree a] | Sequence (Sequence f (ScheduledTree a)) | Single (ScheduledTree a)