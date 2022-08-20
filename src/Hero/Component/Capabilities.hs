module Hero.Component.Capabilities where
import Control.Monad.IO.Class
import Hero.Entity ( Entity ) 

class ComponentGet component store where
  componentContains :: store component -> Entity -> IO Bool
  componentGet :: store component -> Entity -> IO component

class ComponentPut component store where
  componentPut :: store component -> Entity -> component -> IO ()

class ComponentDelete component store where
  componentDelete :: store component -> Entity -> IO ()

class ComponentGet component store => ComponentIterate component store where
  componentIterate :: MonadIO m => store component -> (Entity -> component -> m ()) -> m ()
  componentMembers :: store component -> IO Int