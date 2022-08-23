module Hero.Component.Capabilities where
import Control.Monad.IO.Class
import Hero.Entity ( Entity ) 

-- | You can get component values from stores which support ComponentGet 
class ComponentGet component store where
  componentContains :: store component -> Entity -> IO Bool
  componentGet :: store component -> Entity -> IO component

-- | You can put component values into stores which support ComponentPut.
-- For example, the store of Position2D supports ComponentPut. However, you cannot put
-- Entity since you entity ids should be immutable.
class ComponentPut component store where
  componentPut :: store component -> Entity -> component -> IO ()


-- | You can delete component values from stores which support ComponentDelete.
-- For example, the store of Position2D supports ComponentDelete. However, you cannot delete
-- the Entity component since every entity needs an Id.
class ComponentDelete component store where
  componentDelete :: store component -> Entity -> IO ()


-- | You can iterate (for example with cmap_/cfor) on components whose stores support ComponentIterate.
class ComponentGet component store => ComponentIterate component store where
  componentIterate :: MonadIO m => store component -> (Entity -> component -> m ()) -> m ()
  componentMembers :: store component -> IO Int