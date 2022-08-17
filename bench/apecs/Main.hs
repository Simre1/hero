{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs
import Control.Monad
import Test.Tasty.Bench

data Position = Position {-# UNPACK #-} !Int {-# UNPACK #-} !Int

data Velocity = Velocity {-# UNPACK #-} !Int {-# UNPACK #-} !Int

data Acceleration = Acceleration {-# UNPACK #-} !Int {-# UNPACK #-} !Int

makeWorldAndComponents "World" [''Position, ''Velocity, ''Acceleration]

main :: IO ()
main = do
  w <- initWorld
  runSystem initEntities w

  defaultMain $
    [ bench "simple physics (3 components)" $ whnfIO $ runSystem physics w
    ]

initEntities :: System World ()
initEntities = do
  forM_ [0 .. 1000] $ \i ->
    newEntity (Position 0 i, Velocity 0 0, Acceleration 1 0)

physics :: System World ()
physics = foldl (*>) sys $ const sys <$> [1 .. 20]
  where
    sys =
      cmap (\(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay))
        *> cmap (\(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy))
