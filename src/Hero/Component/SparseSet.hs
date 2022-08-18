module Hero.Component.SparseSet where

import Data.Coerce (coerce)
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Hero.Component
  ( Component,
    ComponentDelete (..),
    ComponentGet (..),
    ComponentIterate (..),
    ComponentMakeStore (..),
    ComponentPut (..),
    MakeStore (MakeStore),
  )
import Hero.Entity
  ( Entity (Entity),
    MaxEntities (MaxEntities),
  )
import Hero.SparseSet.Boxed qualified as SB
import Hero.SparseSet.Storable qualified as SV
import Hero.SparseSet.Unboxed qualified as SU

-- | Component store backed by an unboxed sparse set
newtype SparseSetUnboxedStore a = SparseSetUnboxedStore (SU.SparseSetUnboxed a)

instance (Unbox a) => ComponentGet a SparseSetUnboxedStore where
  storeContains (SparseSetUnboxedStore set) entity = SU.contains set (coerce entity)
  storeGet (SparseSetUnboxedStore set) entity = SU.unsafeLookup set (coerce entity)
  {-# INLINE storeContains #-}
  {-# INLINE storeGet #-}

instance (Unbox a) => ComponentPut a SparseSetUnboxedStore where
  storePut (SparseSetUnboxedStore set) entity val = SU.insert set (coerce entity) val
  {-# INLINE storePut #-}

instance (Unbox a) => ComponentDelete a SparseSetUnboxedStore where
  storeDelete (SparseSetUnboxedStore set) entity = SU.remove set (coerce entity)
  {-# INLINE storeDelete #-}

instance (Unbox a) => ComponentIterate a SparseSetUnboxedStore where
  storeFor (SparseSetUnboxedStore set) f = SU.for set (coerce f)
  storeMembers (SparseSetUnboxedStore set) = SU.size set
  {-# INLINE storeFor #-}
  {-# INLINE storeMembers #-}

instance (Unbox a) => ComponentMakeStore a SparseSetUnboxedStore where
  makeStore (MakeStore global component) = SparseSetUnboxedStore <$> SU.create global component

-- | Component store backed by a storable sparse set
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

-- | Component store backed by a boxed sparse set. The storable version is faster and should be
-- used when possible.
newtype SparseSetBoxedStore a = SparseSetBoxedStore (SB.SparseSetBoxed a)

instance (Storable a) => ComponentGet a SparseSetBoxedStore where
  storeContains (SparseSetBoxedStore set) entity = SB.contains set (coerce entity)
  storeGet (SparseSetBoxedStore set) entity = SB.unsafeLookup set (coerce entity)
  {-# INLINE storeContains #-}
  {-# INLINE storeGet #-}

instance (Storable a) => ComponentPut a SparseSetBoxedStore where
  storePut (SparseSetBoxedStore set) entity val = SB.insert set (coerce entity) val
  {-# INLINE storePut #-}

instance (Storable a) => ComponentDelete a SparseSetBoxedStore where
  storeDelete (SparseSetBoxedStore set) entity = SB.remove set (coerce entity)
  {-# INLINE storeDelete #-}

instance (Storable a) => ComponentIterate a SparseSetBoxedStore where
  storeFor (SparseSetBoxedStore set) f = SB.for set (coerce f)
  storeMembers (SparseSetBoxedStore set) = SB.size set
  {-# INLINE storeFor #-}
  {-# INLINE storeMembers #-}

instance (Storable a) => ComponentMakeStore a SparseSetBoxedStore where
  makeStore (MakeStore global component) = SparseSetBoxedStore <$> SB.create global component
