{-# LANGUAGE TypeFamilies #-}

module Hero.SDL2.Render
  ( runGraphics,
    Graphics (..),
    GraphicsConfig (..),
    SDL.WindowConfig (..),
    defaultGraphics,
    Render (..),
    render,
    Sprite (..),
    TextureSprite(..),
    SDL.Texture,
    loadTexture,
    Shape (..),
    Fill (..),
    fillBorder,
    fillColor,
    Camera (..),
    defaultCamera,
  )
where

import Control.Category (Category (id, (.)), (>>>))
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Text (Text, pack)
import Data.Vector.Storable qualified as Vector
import Data.Word (Word8)
import GHC.Generics (Generic)
import Hero
  ( Arrow (arr, first, second, (&&&)),
    BoxedSparseSet,
    Component (Store),
    Global,
    Position2D (..),
    Rectangle (Rectangle),
    Rotation2D (..),
    System,
    addGlobal,
    cmapM',
    getGlobal,
    liftSystem,
    once,
    withSetup,
  )
import Linear.V2 (V2 (..))
import Linear.V4 (V4 (..))
import Optics.Core ((^.))
import SDL qualified
import SDL.Image qualified as Image
import SDL.Primitive qualified as GFX
import SDL.Video qualified as SDL
import SDL.Video.Renderer qualified as SDL
import Prelude hiding (id, (.))

-- | The graphics context containing the window and the renderer.
-- It can be accessed globally since its stored in a `Global` store.
data Graphics = Graphics
  { renderer :: {-# UNPACK #-} !SDL.Renderer,
    window :: {-# UNPACK #-} !SDL.Window
  }
  deriving (Generic)

-- | All entities with a Render component get rendered.
-- Rendering position is its Position2D + offset
-- Rotation is its Rotation2D + rotation
data Render = Render
  { rotation :: {-# UNPACK #-} !Float,
    offset :: {-# UNPACK #-} !(V2 Float),
    sprite :: {-# UNPACK #-} !Sprite
  }
  deriving (Generic)

-- | Default render with no rotation, no offset and a blue circle as a sprite.
render :: Render
render = Render 0 (V2 0 0) (Shape (SCircle 25) $ Fill (Just $ V4 100 140 200 255) Nothing)

-- | Fill colors used to draw shapes
data Fill = Fill
  { color :: {-# UNPACK #-} !(Maybe (V4 Word8)),
    border :: {-# UNPACK #-} !(Maybe (V4 Word8))
  }
  deriving (Generic)


fillColor :: V4 Word8 -> Fill
fillColor c = Fill {color = Just c, border = Nothing}

fillBorder :: V4 Word8 -> Fill
fillBorder c = Fill {color = Nothing, border = Just c}

-- | The supported shapes
data Shape
  = SRectangle {size :: (V2 Float)}
  | SCircle {radius :: Float}
  deriving (Generic)

-- A sprite can either be a shape or a texture
data Sprite
  = Shape {shape :: Shape, fill :: Fill}
  | Texture TextureSprite
  deriving (Generic)

-- A texture sprite defines a section of the texture which will be rendered
data TextureSprite = TextureSprite
  { size :: V2 Float,
    source :: Maybe (Rectangle Float),
    texture :: SDL.Texture
  }
  deriving (Generic)

instance Component Render where
  type Store Render = BoxedSparseSet

instance Component Graphics where
  type Store Graphics = Global

-- | The Camera defines which portion of the world should be rendered. It is in a `Global` store.
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

renderEntities :: MonadIO m => (Graphics, V2 Float, V2 Float -> V2 Float) -> (Maybe Position2D, Maybe Rotation2D, Render) -> m ()
renderEntities (Graphics renderer window, scale, adjustPosition) (maybePosition, maybeRotation, render :: Render) = do
  let Position2D x y = fromMaybe (Position2D 0 0) maybePosition
      worldPosition = V2 x y + render ^. #offset
      windowPosition = adjustPosition worldPosition
      rotationRadian = fromMaybe 0 (coerce maybeRotation) + render ^. #rotation
  case render ^. #sprite of
    Texture (TextureSprite size source texture) -> do
      let scaledSize@(V2 sX sY) = size * scale
          sourceRect = (\(Rectangle pos src) -> round <$> SDL.Rectangle (SDL.P pos) src) <$> source
          destRect = round <$> SDL.Rectangle (SDL.P $ windowPosition - 0.5 * scaledSize) scaledSize
          rotationDegrees = realToFrac $ rotationRadian * (180 / pi)
      SDL.copyEx renderer texture sourceRect (Just destRect) rotationDegrees Nothing (V2 False False)
    Shape shape fill -> do
      let fillColor = fill ^. #color
          borderColor = fill ^. #border
      case shape of
        SRectangle size ->
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
        SCircle radius -> do
          let r = round radius
              pos = round <$> windowPosition
          maybe (pure ()) (GFX.fillCircle renderer pos r) fillColor
          maybe (pure ()) (GFX.circle renderer pos r) borderColor

loadTexture :: MonadIO m => FilePath -> System m i SDL.Texture
loadTexture filepath = getGlobal @Graphics >>> once (liftSystem $ \graphics -> Image.loadTexture (graphics ^. #renderer) filepath)
{-# INLINE loadTexture #-}