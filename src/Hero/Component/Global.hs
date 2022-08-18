module Hero.Component.Global where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default (Default (def))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Hero.Component
  ( ComponentGet (..),
    ComponentIterate (..),
    ComponentMakeStore (..),
    ComponentPut (..),
  )
import Hero.Entity (entityAmount, forEntities)

-- | Component store using an IORef for a single component instance.
-- Every entity has the same component. Can be used as a 'Store'.
-- The default 'makeStore' needs a 'Default' instance on 'a'. 
-- You can create a custom 'Global' instance which does not need 'Default'.
newtype Global a = Global (IORef a)

-- | Creates a global component store. 
makeGlobal :: a -> IO (Global a)
makeGlobal a = Global <$> newIORef a

instance ComponentGet a Global where
  componentContains (Global ref) entity = pure True
  componentGet (Global ref) entity = readIORef ref
  {-# INLINE componentContains #-}
  {-# INLINE componentGet #-}

instance ComponentPut a Global where
  componentPut (Global ref) entity val = writeIORef ref val
  {-# INLINE componentPut #-}

instance ComponentIterate a Global where
  componentIterate entities (Global ref) f =
    liftIO (readIORef ref)
      >>= \a -> forEntities entities $ \e -> f e a
  componentMembers entities (Global set) = entityAmount entities
  {-# INLINE componentIterate #-}
  {-# INLINE componentMembers #-}

instance Default a => ComponentMakeStore a Global where
  componentMakeStore _ = makeGlobal def
