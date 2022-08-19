module Hero.Component.Store.Global where

import Control.Arrow (Arrow (arr, (***)), (>>>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default (Default (def))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Hero.Component
  ( Component (Store),
    ComponentGet (..),
    ComponentIterate (..),
    ComponentMakeStore (..),
    ComponentPut (..),
    ComponentStore (..),
    Store',
  )
import Hero.System (System, liftSystem, withSetup)
import Hero.World (worldComponent)

-- | Component store using an IORef for a single component instance.
-- Every entity has the same component. Can be used as a 'Store'.
-- The default 'makeStore' needs a 'Default' instance on 'a'.
-- You can create a custom 'Global' instance which does not need 'Default'.
newtype Global a = Global (IORef a)

-- | Creates a global component store.
makeGlobal :: a -> IO (Global a)
makeGlobal a = Global <$> newIORef a

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

instance Default a => ComponentMakeStore a Global where
  componentMakeStore _ = makeGlobal def

getGlobal ::
  forall component m.
  (MonadIO m, Component component, Store component ~ Global) =>
  System m () component
getGlobal =
  withSetup (worldComponent @component)
    >>> liftSystem (\(Global ref) -> liftIO $ readIORef ref)

putGlobal ::
  forall component m.
  (MonadIO m, Component component, Store component ~ Global) =>
  System m component ()
putGlobal =
  arr (\a -> ((), a))
    >>> (withSetup (worldComponent @component)) *** arr id
    >>> liftSystem (\(Global ref, i) -> liftIO $ writeIORef ref i)