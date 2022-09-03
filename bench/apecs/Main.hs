{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

import Apecs
import Control.Monad
import Data.Foldable (for_)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Test.Tasty.Bench

import Apecs.Stores (StorableSparseSet)
import Data.Bits (shiftL, shiftR, (.&.))

data Position = Position {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)
data Velocity = Velocity {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)
data Acceleration = Acceleration {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Generic)

instance GStorable Position
instance GStorable Velocity
instance GStorable Acceleration

instance Component Position where
  type Storage Position = StorableSparseSet Position

instance Component Velocity where
  type Storage Velocity = StorableSparseSet Velocity

instance Component Acceleration where
  type Storage Acceleration = StorableSparseSet Acceleration

makeWorld "World" [''Position, ''Velocity, ''Acceleration]

main :: IO ()
main = do
  w <- initWorld
  runSystem initEntities w

  defaultMain $
    [ bench "simple physics (3 components)" $ whnfIO $ runSystem physics w
    ]

initEntities :: System World ()
initEntities = for_ [0 .. 1000] $ \i ->
  newEntity (Position 0 i, Velocity 0 0, Acceleration 1 0)

physics :: System World ()
physics = do
  for_ [0 .. 999] $ \_ -> do
    cmap $ \(Velocity vx vy, Acceleration ax ay) -> Velocity (vx + ax) (vy + ay)
    cmap $ \(Position x y, Velocity vx vy) -> Position (x + vx) (y + vy)
    pure ()

  pure ()