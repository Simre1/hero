module Hex.Internal.Component.SparseSet (Unboxable, unboxedSet, Storable, storedSet) where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Kind
import Data.SparseSet.Unboxed qualified as SU
import Data.Vector.Unboxing (Unboxable)
import Hex.Internal.Component
import Hex.Internal.Entity
import Data.Vector.Storable (Storable)
import qualified Data.SparseSet.Storable as SV

unboxedSet :: Unboxable component => MaxEntities -> IO (Store component)
unboxedSet (MaxEntities entities) = do
  set <- SU.create entities (entities `quot` 3)
  pure $ Store set

instance Unboxable a => StoreClass SU.SparseSetUnboxed a where
  storeClassContains set entity = SU.contains set (coerce entity)
  storeClassGet set entity = SU.unsafeLookup set (coerce entity)
  storeClassPut set entity val = SU.insert set (coerce entity) val
  storeClassDelete set entity = SU.remove set (coerce entity)
  storeClassFor set f = SU.for set (coerce f)
  storeClassMembers set = SU.size set

storedSet :: Storable component => MaxEntities -> IO (Store component)
storedSet (MaxEntities entities) = do
  set <- SV.create entities (entities `quot` 3)
  pure $ Store set

instance Storable a => StoreClass SV.SparseSetStorable a where
  storeClassContains set entity = SV.contains set (coerce entity)
  storeClassGet set entity = SV.unsafeLookup set (coerce entity)
  storeClassPut set entity val = SV.insert set (coerce entity) val
  storeClassDelete set entity = SV.remove set (coerce entity)
  storeClassFor set f = SV.for set (coerce f)
  storeClassMembers set = SV.size set
  -- {-# INLINE storeClassContains #-}
  -- {-# INLINE storeClassGet #-}
  -- {-# INLINE storeClassPut #-}
  -- {-# INLINE storeClassDelete #-}
  -- {-# INLINE storeClassFor #-}
  -- {-# INLINE storeClassMembers #-}      
