{-# LANGUAGE TypeFamilies #-}

module Hero.SDL2.Render where

import Control.Category
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text
import Data.Word
import GHC.Generics
import Hero
import Linear.V2
import Linear.V4
import Optics.Core
import SDL qualified
import SDL.Video qualified as SDL
import SDL.Video.Renderer qualified as SDL
import Prelude hiding (id, (.))

data Graphics = Graphics
  { renderer :: {-# UNPACK #-} !SDL.Renderer,
    window :: {-# UNPACK #-} !SDL.Window
  }
  deriving (Generic)

data Render = Render
  { rotation :: {-# UNPACK #-} !Rotation,
    size :: {-# UNPACK #-} !(V2 Float),
    offset :: {-# UNPACK #-} !(V2 Float),
    image :: {-# UNPACK #-} !Image
  }
  deriving (Generic)

data Rotation = Rotation deriving (Generic)

data Fill = Fill
  { color :: {-# UNPACK #-} !(Maybe (V4 Word8))
  }
  deriving (Generic)

data Shape = Rectangle deriving (Generic)

data Image = Shape Shape Fill deriving (Generic)

instance Component Render where
  type Store Render = BoxedSparseSet

instance Component Graphics where
  type Store Graphics = Global

data Camera = Camera
  { size :: V2 Float,
    position :: V2 Float
  }
  deriving (Generic)

instance Component Camera where
  type Store Camera = Global

createWindow :: GraphicsConfig -> IO Graphics
createWindow graphicsConfig = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (graphicsConfig ^. #windowName) (graphicsConfig ^. #windowConfig)
  renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig (if graphicsConfig ^. #vsync then SDL.AcceleratedVSyncRenderer else SDL.AcceleratedRenderer) False
  pure $ Graphics renderer window

data GraphicsConfig = GraphicsConfig
  { windowName :: Text,
    windowConfig :: SDL.WindowConfig,
    vsync :: Bool
  }
  deriving (Generic)

defaultGraphics :: GraphicsConfig
defaultGraphics =
  GraphicsConfig
    { windowName = pack "Hero App",
      windowConfig = SDL.defaultWindow,
      vsync = True
    }

defaultCamera :: Camera
defaultCamera =
  Camera
    { position = V2 0 0,
      size = V2 800 600
    }

runGraphics :: forall i o m. MonadIO m => GraphicsConfig -> System m i o -> System m i o
runGraphics graphicsConfig system = setup >>> second system >>> first graphics >>> arr snd
  where
    setup :: System m i (Graphics, i)
    setup = withSetup (\_ -> createWindow graphicsConfig) (\graphics -> addGlobal graphics *> addGlobal defaultCamera *> pure graphics) &&& id
    graphics :: System m Graphics ()
    graphics =
      let render =
            (id &&& getGlobal @Camera)
              >>> cmapM'
                ( \(Graphics renderer window, Camera cameraSize cameraPosition) (Position2D x y, render :: Render) -> do
                    windowSize@(V2 _ windowHeight) <- fmap (fmap fromIntegral) $ SDL.get $ SDL.windowSize window
                    let scale = windowSize / cameraSize
                        size = render ^. #size * scale
                        virtualPosition = V2 x y + render ^. #offset
                        windowPosition = (V2 id (\y -> windowHeight - y) <*> (virtualPosition * scale + windowSize / 2)) - size / 2
                        position = SDL.P $ fmap round $ windowPosition
                    case render ^. #image of
                      Shape shape fill -> do
                        let color = fromMaybe (V4 0 0 0 255) $ fill ^. #color
                        SDL.rendererDrawColor renderer SDL.$= color
                        case shape of
                          Rectangle -> SDL.fillRect renderer (Just $ SDL.Rectangle position (round <$> size))
                )
          present = liftSystem (\g@(Graphics renderer _) -> SDL.present renderer)
          clear = liftSystem (\g@(Graphics renderer _) -> SDL.rendererDrawColor renderer SDL.$= (V4 0 0 0 255) >> SDL.clear renderer *> pure g)
       in clear >>> id &&& render >>> arr fst >>> present
{-# INLINE runGraphics #-}