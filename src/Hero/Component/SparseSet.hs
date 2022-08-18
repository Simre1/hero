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
  componentContains (UnboxedSparseSet set) entity = SU.contains set (coerce entity)
  componentGet (UnboxedSparseSet set) entity = SU.unsafeLookup set (coerce entity)
  {-# INLINE componentContains #-}
  {-# INLINE componentGet #-}

instance (Unbox a) => ComponentPut a UnboxedSparseSet where
  componentPut (UnboxedSparseSet set) entity val = SU.insert set (coerce entity) val
  {-# INLINE componentPut #-}

instance (Unbox a) => ComponentDelete a UnboxedSparseSet where
  componentDelete (UnboxedSparseSet set) entity = SU.remove set (coerce entity)
  {-# INLINE componentDelete #-}

instance (Unbox a) => ComponentIterate a UnboxedSparseSet where
  componentIterate (UnboxedSparseSet set) f = SU.for set (coerce f)
  componentMembers (UnboxedSparseSet set) = SU.size set
  {-# INLINE componentIterate #-}
  {-# INLINE componentMembers #-}


instance (Unbox a) => ComponentMakeStore a UnboxedSparseSet where
  componentMakeStore (MaxEntities global) = unboxedSparseSet global global

-- | Component store backed by a storable sparse set. Can be used as a 'Store'.
newtype StorableSparseSet a = StorableSparseSet (SV.SparseSetStorable a)

instance (Storable a) => ComponentGet a StorableSparseSet where
  componentContains (StorableSparseSet set) entity = SV.contains set (coerce entity)
  componentGet (StorableSparseSet set) entity = SV.unsafeLookup set (coerce entity)
  {-# INLINE componentContains #-}
  {-# INLINE componentGet #-}

instance (Storable a) => ComponentPut a StorableSparseSet where
  componentPut (StorableSparseSet set) entity val = SV.insert set (coerce entity) val
  {-# INLINE componentPut #-}

instance (Storable a) => ComponentDelete a StorableSparseSet where
  componentDelete (StorableSparseSet set) entity = SV.remove set (coerce entity)
  {-# INLINE componentDelete #-}

instance (Storable a) => ComponentIterate a StorableSparseSet where
  componentIterate (StorableSparseSet set) f = SV.for set (coerce f)
  componentMembers (StorableSparseSet set) = SV.size set
  {-# INLINE componentIterate #-}
  {-# INLINE componentMembers #-}

-- | Creates a storable sparse set. The first parameter should be the maximum amount of live (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
storableSparseSet :: Storable a => Word32 -> Word32 -> IO (StorableSparseSet a)
storableSparseSet global component = StorableSparseSet <$> SV.create global component

instance (Storable a) => ComponentMakeStore a StorableSparseSet where
  componentMakeStore (MaxEntities global) = storableSparseSet global global

-- | Component store backed by a boxed sparse set. Can be used as a 'Store'. The storable version is faster and should be
-- used when possible.
newtype BoxedSparseSet a = BoxedSparseSet (SB.SparseSetBoxed a)

instance (Storable a) => ComponentGet a BoxedSparseSet where
  componentContains (BoxedSparseSet set) entity = SB.contains set (coerce entity)
  componentGet (BoxedSparseSet set) entity = SB.unsafeLookup set (coerce entity)
  {-# INLINE componentContains #-}
  {-# INLINE componentGet #-}

instance (Storable a) => ComponentPut a BoxedSparseSet where
  componentPut (BoxedSparseSet set) entity val = SB.insert set (coerce entity) val
  {-# INLINE componentPut #-}

instance (Storable a) => ComponentDelete a BoxedSparseSet where
  componentDelete (BoxedSparseSet set) entity = SB.remove set (coerce entity)
  {-# INLINE componentDelete #-}

instance (Storable a) => ComponentIterate a BoxedSparseSet where
  componentIterate (BoxedSparseSet set) f = SB.for set (coerce f)
  componentMembers (BoxedSparseSet set) = SB.size set
  {-# INLINE componentIterate #-}
  {-# INLINE componentMembers #-}

-- | Creates a boxed sparse set. The first parameter should be the maximum amount of live entities (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
boxedSparseSet :: Storable a => Word32 -> Word32 -> IO (BoxedSparseSet a)
boxedSparseSet global component = BoxedSparseSet <$> SB.create global component

instance (Storable a) => ComponentMakeStore a BoxedSparseSet where
  componentMakeStore (MaxEntities global) = boxedSparseSet global global
