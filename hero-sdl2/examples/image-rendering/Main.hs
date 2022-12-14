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

system :: System () ()
system =
  runGraphics defaultGraphics $
    addTimingComponents >>> do
      loadTexture "image.jpg"
        >>> once
          ( liftSystem (pure . sprite) >>> createEntity
          )
      getGlobal @Timer
        >>> cmap
          ( \(Timer t) (render :: Render) ->
              render
                & #sprite
                  % #_Texture
                  % #size
                    .~ ((* (1.5 + 0.5 * (sin t))) <$> V2 800 600)
          )
      pure ()

sprite :: Texture -> Render
sprite texture =
  render & #sprite
    .~ ( Texture $
           TextureSprite
             { size = V2 0 0,
               source = Nothing,
               texture
             }
       )