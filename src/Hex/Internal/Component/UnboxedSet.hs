module Hex.Internal.Component.UnboxedSet (Unboxable, unboxedSet) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Kind
import Data.SparseSet.Unboxed qualified as SU
import Data.Vector.Unboxing (Unboxable)
import Hex.Internal.Component
import Hex.Internal.Entity

unboxedSet :: Unboxable component => MaxEntities -> IO (Store component)
unboxedSet (MaxEntities entities) = do
  set <- SU.create entities (entities `quot` 3)
  pure $ Store $ UnboxableStore set


newtype UnboxableStore a = UnboxableStore (SU.SparseSetUnboxed a) 

instance Unboxable a => StoreClass UnboxableStore a where
  storeClassContains (UnboxableStore set) entity = SU.contains set (coerce entity)
  storeClassGet (UnboxableStore set) entity = SU.unsafeLookup set (coerce entity)
  storeClassPut (UnboxableStore set) entity val = SU.insert set (coerce entity) val
  storeClassDelete (UnboxableStore set) entity = SU.remove set (coerce entity)
  storeClassFor (UnboxableStore set) f = SU.for set (coerce f)
  storeClassMembers (UnboxableStore set) = SU.size set
  -- {-# INLINE storeClassContains #-}
  -- {-# INLINE storeClassGet #-}
  -- {-# INLINE storeClassPut #-}
  -- {-# INLINE storeClassDelete #-}
  -- {-# INLINE storeClassFor #-}
  -- {-# INLINE storeClassMembers #-}      
