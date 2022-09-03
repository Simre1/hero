{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad (forever)
import Data.Foldable (Foldable (fold), for_)
import Data.Monoid
import Hero
import Hero.SDL2.Input
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

system :: System () ()
system =
  runGraphics defaultGraphics $
    addSDLEvents >>> addTimingComponents >>> do
      once
        ( pure (Position2D 0 0, sprite) >>> createEntity *>
          pure (Position2D 100 0, sprite) >>> createEntity
        )
      getGlobal @TimeDelta &&& getKeyboardState
        >>> cmap
          ( \(TimeDelta dt, keyboardState) (Position2D x y) ->
              let keys = [ScancodeUp, ScancodeDown, ScancodeLeft, ScancodeRight]
                  directions = [V2 0 1, V2 0 (-1), V2 (-1) 0, V2 1 0]
                  (Sum (V2 dx dy)) =
                    fold $
                      fmap
                        ( \(pressed, dir) ->
                            Sum $
                              if pressed then dir else V2 0 0
                        )
                        $ (isPressed keyboardState <$> keys) `zip` directions
               in Position2D (x + dx * dt * speed) (y + dy * dt * speed)
          )
      pure ()

speed :: Float
speed = 200

sprite :: Render
sprite =
  Render
    { rotation = 0,
      offset = 0,
      sprite =
        Shape
          { shape = (SCircle 50),
            fill = (fillColor $ V4 150 150 30 255)
          }
    }