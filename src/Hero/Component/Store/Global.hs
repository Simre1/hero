module Hero.Component.Store.Global where

import Control.Arrow (Arrow (arr, (***)), (>>>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Hero.Component.Capabilities
    ( ComponentPut(..), ComponentGet(..) )
import Hero.Component.Component
    ( Component(..), ComponentStore(..) )
import Hero.System.System
    ( System, liftSystem, withSetup, withSetup' )
import Hero.System.ComponentFunctions (addStore)
import Hero.World qualified as World

-- | Component store using an IORef for a single component instance.
-- Every entity has the same component. Can be used as a 'Store'.
-- The default 'makeStore' needs a 'Default' instance on 'a'.
-- You can create a custom 'Global' instance which does not need 'Default'.
newtype Global a = Global (IORef a)

-- | Creates a global component store.
addGlobal :: (Component a, Global ~ Store a) => MonadIO m => a -> System m i i
addGlobal a = withSetup (\_ -> Global <$> newIORef a) addStore

instance ComponentStore a Global where
  componentEntityDelete _ _ = pure ()

instance ComponentGet a Global where
  componentContains (Global ref) entity = pure True
  componentGet (Global ref) entity = readIORef ref
  {-# INLINE componentContains #-}
  {-# INLINE componentGet #-}

instance ComponentPut a Global where
  componentPut (Global ref) entity val = writeIORef ref val
  {-# INLINE componentPut #-}


-- Gets the value of a global component store
getGlobal ::
  forall component m i.
  (MonadIO m, Component component, Store component ~ Global) =>
  System m i component
getGlobal =
  withSetup' (World.getStore @component)
    >>> liftSystem (\(Global ref) -> liftIO $ readIORef ref)

-- Puts the value into a global component store.
putGlobal ::
  forall component m.
  (MonadIO m, Component component, Store component ~ Global) =>
  System m component ()
putGlobal =
  arr (\a -> ((), a))
    >>> (withSetup' (World.getStore @component)) *** arr id
    >>> liftSystem (\(Global ref, i) -> liftIO $ writeIORef ref i)