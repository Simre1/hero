{-# LANGUAGE TypeFamilies #-}

import Control.Monad (forM_)
import Data.Foldable (for_)
import Debug.Trace
import Hero
import Hero.System.System

data Position = Position Int Int deriving Show

data Velocity = Velocity Int Int

instance Component Position where
  type Store Position = BoxedSparseSet

instance Component Velocity where
  type Store Velocity = BoxedSparseSet

main :: IO ()
main = do
  world <- createWorld 10000
  runSystem <- compileSystem system world
  runSystem ()

system :: System () ()
system =
  -- Create two entities
  (pure (Position 0 0, Velocity 1 0) >>> createEntity)
    *> (pure (Position 10 0, Velocity 0 1) >>> createEntity)
    *>
    -- Map position 10 times
    for_
      [1 .. 10]
      ( \_ ->
          cmap_ (\(Position x y, Velocity vx vy) -> traceShowId $ Position (x + vx) (y + vy))
      )
    *>
    -- Print the current position
    cmapM_ (\(Position x y) -> print (x, y))
