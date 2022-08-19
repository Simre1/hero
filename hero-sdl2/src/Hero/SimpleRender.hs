{-# LANGUAGE TypeFamilies #-}

module Hero.SimpleRender where

import Control.Category
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text
import Data.Word
import GHC.Generics
import Hero
import Hero.World (worldAddComponent)
import Linear.V2
import Linear.V4
import SDL qualified
import SDL.Video qualified as SDL
import SDL.Video.Renderer qualified as SDL
import Prelude hiding (id, (.))

data Graphics = Graphics
  { graphicsRenderer :: {-# UNPACK #-} !SDL.Renderer,
    graphicsWindow :: {-# UNPACK #-} !SDL.Window
  }

data Render = Render
  { renderRotation :: {-# UNPACK #-} !Rotation,
    renderSize :: {-# UNPACK #-} !(V2 Float),
    renderOffset :: {-# UNPACK #-} !(V2 Float),
    renderImage :: {-# UNPACK #-} !Image
  }

data Rotation = Rotation

data Fill = Fill
  { fillColor :: {-# UNPACK #-} !(Maybe (V4 Word8))
  }

data Shape = Rectangle

data Image = Shape Shape Fill

instance Component Render where
  type Store Render = BoxedSparseSet

instance Component Graphics where
  type Store Graphics = Global
  makeStore _ = ManualMakeStore

createWindow :: GraphicsConfig -> IO Graphics
createWindow graphicsConfig = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (windowName graphicsConfig) (windowConfig graphicsConfig)
  renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig (if graphicsVSync graphicsConfig then SDL.AcceleratedVSyncRenderer else SDL.AcceleratedRenderer) False
  pure $ Graphics renderer window

data GraphicsConfig = GraphicsConfig
  { windowName :: Text,
    windowConfig :: SDL.WindowConfig,
    graphicsVSync :: Bool,
    virtualWindowSize :: V2 Int
  }

defaultGraphics :: GraphicsConfig
defaultGraphics = GraphicsConfig {
  windowName = pack "Hero App",
  windowConfig = SDL.defaultWindow,
  graphicsVSync = True,
  virtualWindowSize = V2 800 600
}

runGraphics :: forall i o m. MonadIO m => GraphicsConfig -> System m i o -> System m i o
runGraphics graphicsConfig system = setup &&& id >>> second system >>> first graphics >>> arr snd
  where
    setup :: System m i Graphics
    setup = withSetup $ \world -> do
      graphics <- createWindow graphicsConfig
      makeGlobal graphics >>= worldAddComponent @Graphics world
      pure graphics
    graphics :: System m Graphics ()
    graphics =
      let render = cmapM' $ \(Graphics renderer window) (Position2D x y, render :: Render) -> do
            windowSize <- fmap (fmap fromIntegral) $ SDL.get $ SDL.windowSize window
            let virtualWindow = fromIntegral <$> virtualWindowSize graphicsConfig
                scale = windowSize / virtualWindow
                position = SDL.P $ fmap round $ V2 x y + renderOffset render
                size = round <$> renderSize render * scale
            case renderImage render of
              Shape shape fill -> do
                let color = fromMaybe (V4 0 0 0 255) $ fillColor fill
                SDL.rendererDrawColor renderer SDL.$= color
                case shape of
                  Rectangle -> SDL.fillRect renderer (Just $ SDL.Rectangle position size)
          present = liftSystem (\g@(Graphics renderer _) -> SDL.present renderer)
          clear = liftSystem (\g@(Graphics renderer _) -> SDL.rendererDrawColor renderer SDL.$= (V4 0 0 0 255) >> SDL.clear renderer *> pure g)
      in clear >>> id &&& render >>> arr fst >>> present
{-# INLINE runGraphics #-}