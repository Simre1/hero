module Main where

  
import Hero
import Hero.SimpleRender
import Linear.V4
import Linear.V2


import Control.Monad (forever)
import Data.Foldable (for_)

main :: IO ()
main = do
  world <- createWorld 1000

  runStartup <- compileSystem startup world
  runStartup ()

  run <- compileSystem system world
  forever $ do
    run ()

startup :: System IO () ()
startup = for_ [1..5] $ \i -> 
  pure (Position2D (i * 100) 100, rect) >>> newEntity

system :: System IO () ()
system = runGraphics defaultGraphics $ 
    cmap (\(Position2D x y) -> Position2D (x+1) (y+1))

rect :: Render
rect = Render {
  renderRotation = Rotation,
  renderSize = V2 50 50,
  renderOffset = V2 0 0,
  renderImage = Shape Rectangle (Fill $ Just (V4 100 0 0 255))
}