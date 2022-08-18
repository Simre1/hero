module Hero.Component.SparseSet (
  StorableSparseSet,
  storableSparseSet,
  UnboxedSparseSet,
  unboxedSparseSet,
  BoxedSparseSet,
  boxedSparseSet
) where

import Data.Coerce (coerce)
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Data.Word (Word32)
import Hero.Component
  ( Component,
    ComponentDelete (..),
    ComponentGet (..),
    ComponentIterate (..),
    ComponentMakeStore (..),
    ComponentPut (..),
  )
import Hero.Entity
  ( Entity (Entity),
    MaxEntities (MaxEntities),
  )
import Hero.SparseSet.Boxed qualified as SB
import Hero.SparseSet.Storable qualified as SV
import Hero.SparseSet.Unboxed qualified as SU

-- | Component store backed by an unboxed sparse set. Can be used as a 'Store'.
newtype UnboxedSparseSet a = UnboxedSparseSet (SU.SparseSetUnboxed a)

-- | Creates an unboxed sparse set. The first parameter should be the maximum amount of live entities (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
unboxedSparseSet :: Unbox a => Word32 -> Word32 -> IO (UnboxedSparseSet a)
unboxedSparseSet global component = UnboxedSparseSet <$> SU.create global component

instance (Unbox a) => ComponentGet a UnboxedSparseSet where
  storeContains (UnboxedSparseSet set) entity = SU.contains set (coerce entity)
  storeGet (UnboxedSparseSet set) entity = SU.unsafeLookup set (coerce entity)
  {-# INLINE storeContains #-}
  {-# INLINE storeGet #-}

instance (Unbox a) => ComponentPut a UnboxedSparseSet where
  storePut (UnboxedSparseSet set) entity val = SU.insert set (coerce entity) val
  {-# INLINE storePut #-}

instance (Unbox a) => ComponentDelete a UnboxedSparseSet where
  storeDelete (UnboxedSparseSet set) entity = SU.remove set (coerce entity)
  {-# INLINE storeDelete #-}

instance (Unbox a) => ComponentIterate a UnboxedSparseSet where
  storeFor (UnboxedSparseSet set) f = SU.for set (coerce f)
  storeMembers (UnboxedSparseSet set) = SU.size set
  {-# INLINE storeFor #-}
  {-# INLINE storeMembers #-}


instance (Unbox a) => ComponentMakeStore a UnboxedSparseSet where
  makeStore (MaxEntities global) = unboxedSparseSet global global

-- | Component store backed by a storable sparse set. Can be used as a 'Store'.
newtype StorableSparseSet a = StorableSparseSet (SV.SparseSetStorable a)

instance (Storable a) => ComponentGet a StorableSparseSet where
  storeContains (StorableSparseSet set) entity = SV.contains set (coerce entity)
  storeGet (StorableSparseSet set) entity = SV.unsafeLookup set (coerce entity)
  {-# INLINE storeContains #-}
  {-# INLINE storeGet #-}

instance (Storable a) => ComponentPut a StorableSparseSet where
  storePut (StorableSparseSet set) entity val = SV.insert set (coerce entity) val
  {-# INLINE storePut #-}

instance (Storable a) => ComponentDelete a StorableSparseSet where
  storeDelete (StorableSparseSet set) entity = SV.remove set (coerce entity)
  {-# INLINE storeDelete #-}

instance (Storable a) => ComponentIterate a StorableSparseSet where
  storeFor (StorableSparseSet set) f = SV.for set (coerce f)
  storeMembers (StorableSparseSet set) = SV.size set
  {-# INLINE storeFor #-}
  {-# INLINE storeMembers #-}

-- | Creates a storable sparse set. The first parameter should be the maximum amount of live entities (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
storableSparseSet :: Storable a => Word32 -> Word32 -> IO (StorableSparseSet a)
storableSparseSet global component = StorableSparseSet <$> SV.create global component

instance (Storable a) => ComponentMakeStore a StorableSparseSet where
  makeStore (MaxEntities global) = storableSparseSet global global

-- | Component store backed by a boxed sparse set. Can be used as a 'Store'. The storable version is faster and should be
-- used when possible.
newtype BoxedSparseSet a = BoxedSparseSet (SB.SparseSetBoxed a)

instance (Storable a) => ComponentGet a BoxedSparseSet where
  storeContains (BoxedSparseSet set) entity = SB.contains set (coerce entity)
  storeGet (BoxedSparseSet set) entity = SB.unsafeLookup set (coerce entity)
  {-# INLINE storeContains #-}
  {-# INLINE storeGet #-}

instance (Storable a) => ComponentPut a BoxedSparseSet where
  storePut (BoxedSparseSet set) entity val = SB.insert set (coerce entity) val
  {-# INLINE storePut #-}

instance (Storable a) => ComponentDelete a BoxedSparseSet where
  storeDelete (BoxedSparseSet set) entity = SB.remove set (coerce entity)
  {-# INLINE storeDelete #-}

instance (Storable a) => ComponentIterate a BoxedSparseSet where
  storeFor (BoxedSparseSet set) f = SB.for set (coerce f)
  storeMembers (BoxedSparseSet set) = SB.size set
  {-# INLINE storeFor #-}
  {-# INLINE storeMembers #-}

-- | Creates a boxed sparse set. The first parameter should be the maximum amount of live entities (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
boxedSparseSet :: Storable a => Word32 -> Word32 -> IO (BoxedSparseSet a)
boxedSparseSet global component = BoxedSparseSet <$> SB.create global component

instance (Storable a) => ComponentMakeStore a BoxedSparseSet where
  makeStore (MaxEntities global) = boxedSparseSet global global
