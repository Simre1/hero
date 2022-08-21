{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad (forever)
import Data.Foldable (for_)
import Hero
import Hero.SDL2.Render
import Linear.V2
import Linear.V4
import Optics.Core

main :: IO ()
main = do
  world <- createWorld 1000
  run <- compileSystem system world
  forever $ do
    run ()

system :: System IO () ()
system =
  runGraphics defaultGraphics $
    timingComponents >>> do
      once $
        for_ [-1, 0, 1] $ \i ->
          pure (Position2D (i * 150) 0, rect) >>> newEntity
      getGlobal @Timer
        >>> cmap'
          ( \(Timer t) (render :: Render) ->
              let t' = realToFrac t in
              render
                & #offset .~ (V2 (100 * cos (t' * pi)) (100 * sin (t' * pi)))
                & #rotation .~ (t' * pi)
          )
      pure ()

rect :: Render
rect =
  Render
    { rotation = 0,
      offset = V2 0 0,
      image = Shape (Rectangle 50) (Fill (Just (V4 100 0 0 255)) (Just (V4 255 255 255 255)))
    }