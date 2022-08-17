module Hex.Component.SparseSet where

import Data.Coerce (coerce)
import Data.SparseSet.Storable qualified as SV
import Data.SparseSet.Unboxed qualified as SU
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Hex.Component
  ( Component,
    ComponentDelete (..),
    ComponentGet (..),
    ComponentIterate (..),
    ComponentMakeStore (..),
    ComponentPut (..),
    MakeStore (MakeStore),
  )
import Hex.Entity
  ( Entity (Entity),
    MaxEntities (MaxEntities),
  )

-- newtype SparseSetUnboxedStore a = SparseSetUnboxedStore (SU.SparseSetUnboxed a)

-- instance Unbox a => ComponentStore a SparseSetUnboxedStore where
--   storeContains (SparseSetUnboxedStore set) entity = SU.contains set (coerce entity)
--   storeGet (SparseSetUnboxedStore set) entity = SU.unsafeLookup set (coerce entity)
--   storePut (SparseSetUnboxedStore set) entity val = SU.insert set (coerce entity) val
--   storeDelete (SparseSetUnboxedStore set) entity = SU.remove set (coerce entity)
--   storeFor (SparseSetUnboxedStore set) f = SU.for set (coerce f)
--   storeMembers (SparseSetUnboxedStore set) = SU.size set
--   makeStore (MakeStore global component) = SparseSetUnboxedStore <$> SU.create global component
--   {-# INLINE storeContains #-}
--   {-# INLINE storeGet #-}
--   {-# INLINE storePut #-}
--   {-# INLINE storeDelete #-}
--   {-# INLINE storeFor #-}
--   {-# INLINE storeMembers #-}

newtype SparseSetStorableStore a = SparseSetStorableStore (SV.SparseSetStorable a)

instance (Storable a) => ComponentGet a SparseSetStorableStore where
  storeContains (SparseSetStorableStore set) entity = SV.contains set (coerce entity)
  storeGet (SparseSetStorableStore set) entity = SV.unsafeLookup set (coerce entity)
  {-# INLINE storeContains #-}
  {-# INLINE storeGet #-}

instance (Storable a) => ComponentPut a SparseSetStorableStore where
  storePut (SparseSetStorableStore set) entity val = SV.insert set (coerce entity) val
  {-# INLINE storePut #-}

instance (Storable a) => ComponentDelete a SparseSetStorableStore where
  storeDelete (SparseSetStorableStore set) entity = SV.remove set (coerce entity)
  {-# INLINE storeDelete #-}

instance (Storable a) => ComponentIterate a SparseSetStorableStore where
  storeFor (SparseSetStorableStore set) f = SV.for set (coerce f)
  storeMembers (SparseSetStorableStore set) = SV.size set
  {-# INLINE storeFor #-}
  {-# INLINE storeMembers #-}

instance (Storable a) => ComponentMakeStore a SparseSetStorableStore where
  makeStore (MakeStore global component) = SparseSetStorableStore <$> SV.create global component
