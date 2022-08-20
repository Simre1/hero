{-# LANGUAGE TypeFamilies #-}
import Control.Monad ( forM_ )
import Hero
import Data.Foldable (for_)

data Position = Position Int Int

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

system :: System IO () ()
system =
  
  -- Create two entities
  (pure (Position 0 0, Velocity 1 0) >>> newEntity) *>
  (pure (Position 10 0, Velocity 0 1) >>> newEntity) *>
  
  -- Map position 10 times
  for_ [1..10] (\_ -> cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))) *>

  -- Print the current position
  cmapM (\(Position x y) -> print (x,y))
