module Hero.Parallel.ExecutionPlanner where

import Data.Graph qualified as G
import Data.IntMap qualified as IM
import GHC.Generics (Generic)

data Resources a = Resources
  { read :: [a],
    write :: [a]
  }
  deriving (Generic)

noResources :: Resources a
noResources = Resources [] []

data ResourceValue r a = ResourceValue
  { resources :: Resources r,
    value :: a
  }
  deriving (Generic)

-- data ResourceGraph r a = ResourceGraph
--   { graph :: G.Graph,
--     nodes :: IM.IntMap (Node r a)
--   }

-- insertNode :: ResourceGraph -> Int -> ResourceGraph
-- insertNode rs i = rs & #graph

data Combine f a where
  CombineApp :: f (a -> b) -> Combine f a -> Combine f b
  CombinePure :: f a -> Combine f a

-- data Sequence f a where
--   SequenceThen :: f a -> (a -> Sequence f b) -> Sequence f b
--   SequencePure :: f a -> Sequence f a

-- data ExecutionPlan r m a b where
--   Sequence :: ExecutionPlan r m a x -> ExecutionPlan r m x b -> ExecutionPlan r m a b
--   Parallel :: ExecutionPlan r m a (x -> b) -> ExecutionPlan r m a x -> ExecutionPlan r m a b
--   Execute :: Resources r -> (a -> m b) -> ExecutionPlan r m a b
--   MapInput :: (a -> a') -> ExecutionPlan r m a' b -> ExecutionPlan r m a b
--   MapOutput :: (b' -> b) -> ExecutionPlan r m a b' -> ExecutionPlan r m a b
--   Forward :: ExecutionPlan r m a b -> ExecutionPlan r m a a


data ExecutionPlanner r = ExecutionPlanner



-- data ScheduledTree f a = Parallel [Combine ScheduledTree a] | Sequence (Sequence f (ScheduledTree a)) | Single (ScheduledTree a)