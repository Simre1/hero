{-# LANGUAGE TypeFamilies #-}

module Hero.SDL2.Render where

import Control.Category
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Maybe
import Data.Text
import Data.Vector.Storable qualified as Vector
import Data.Word
import Debug.Trace
import GHC.Generics
import Hero
import Linear.V2
import Linear.V4
import Optics.Core
import SDL qualified
import SDL.Primitive qualified as GFX
import SDL.Video qualified as SDL
import SDL.Video.Renderer qualified as SDL
import Prelude hiding (id, (.))

data Graphics = Graphics
  { renderer :: {-# UNPACK #-} !SDL.Renderer,
    window :: {-# UNPACK #-} !SDL.Window
  }
  deriving (Generic)

data Render = Render
  { rotation :: {-# UNPACK #-} !Float,
    offset :: {-# UNPACK #-} !(V2 Float),
    image :: {-# UNPACK #-} !Sprite
  }
  deriving (Generic)

data Fill = Fill
  { fill :: {-# UNPACK #-} !(Maybe (V4 Word8)),
    border :: {-# UNPACK #-} !(Maybe (V4 Word8))
  }
  deriving (Generic)

data Shape = Rectangle {size :: (V2 Float)} | Circle {radius :: Float} deriving (Generic)

data Sprite = Shape Shape Fill deriving (Generic)

instance Component Render where
  type Store Render = BoxedSparseSet

instance Component Graphics where
  type Store Graphics = Global

data Camera = Camera
  { position :: V2 Float,
    size :: V2 Float
  }
  deriving (Generic, Show)

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
              >>> liftSystem
                ( \(graphics@(Graphics renderer window), camera) -> do
                    windowSize@(V2 windowLength windowHeight) <- fmap (fmap fromIntegral) $ SDL.get $ SDL.windowSize window
                    let scale = windowSize / (camera ^. #size)

                    pure (graphics, scale, adjustPosition camera windowSize)
                )
              >>> cmapM' renderEntities
          present = liftSystem (\g@(Graphics renderer _) -> SDL.present renderer)
          clear = liftSystem (\g@(Graphics renderer _) -> SDL.rendererDrawColor renderer SDL.$= (V4 0 0 0 255) >> SDL.clear renderer *> pure g)
       in clear >>> id &&& render >>> arr fst >>> present
{-# INLINE runGraphics #-}

adjustPosition :: Camera -> V2 Float -> V2 Float -> V2 Float
adjustPosition (camera@(Camera (V2 cX cY) cameraSize)) windowSize@(V2 windowLength windowHeight) (V2 x y) =
  let (V2 wX wY) = ((V2 x y) - (V2 cX cY)) * scale
   in 0.5 * windowSize + V2 wX (-wY)
  where
    scale = windowSize / cameraSize

rotatePoint :: Float -> V2 Float -> V2 Float
rotatePoint radian (V2 x y) = V2 (x * cos radian - y * sin radian) (x * sin radian + y * cos radian)

renderEntities :: MonadIO m => (Graphics, V2 Float, V2 Float -> V2 Float) -> (Position2D, Maybe Rotation2D, Render) -> m ()
renderEntities (Graphics renderer window, scale, adjustPosition) (Position2D x y, maybeRotation, render :: Render) = do
  let worldPosition = V2 x y + render ^. #offset
      windowPosition = adjustPosition worldPosition
      rotationRadian = fromMaybe 0 (coerce maybeRotation) + render ^. #rotation
  case render ^. #image of
    Shape shape fill -> do
      let fillColor = fill ^. #fill
          borderColor = fill ^. #border
      case shape of
        Rectangle size ->
          let half@(V2 hx hy) = scale * size / 2
           in if abs rotationRadian < 0.01
                then do
                  let topLeft = windowPosition - half
                      bottomRight = windowPosition + half
                  maybe (pure ()) (GFX.fillRectangle renderer (round <$> topLeft) (round <$> bottomRight)) fillColor
                  maybe (pure ()) (GFX.rectangle renderer (round <$> topLeft) (round <$> bottomRight)) borderColor
                else do
                  let points = (+ windowPosition) . rotatePoint rotationRadian <$> [V2 (-hx) (-hy), V2 (hx) (-hy), V2 (hx) (hy), V2 (-hx) (hy)]
                  let xs = Vector.fromList $ round . (\(V2 x _) -> x) <$> points
                  let ys = Vector.fromList $ round . (\(V2 _ y) -> y) <$> points

                  maybe (pure ()) (GFX.fillPolygon renderer xs ys) fillColor
                  maybe (pure ()) (GFX.polygon renderer xs ys) borderColor
        Circle radius -> do
          let r = round radius
              pos = round <$> windowPosition
          maybe (pure ()) (GFX.fillCircle renderer pos r) fillColor
          maybe (pure ()) (GFX.circle renderer pos r) borderColor

-- Circle radius -> GFX.circle