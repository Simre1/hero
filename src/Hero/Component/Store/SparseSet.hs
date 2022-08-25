{-# LANGUAGE AllowAmbiguousTypes #-}

module Hero.Component.Store.SparseSet where

import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Data.Word (Word32)
import Hero.Component.Capabilities
import Hero.Component.Component
import Hero.Entity
  ( Entity (Entity),
    MaxEntities (MaxEntities),
  )
import Hero.SparseSet.Boxed qualified as SB
import Hero.SparseSet.Storable qualified as SV
import Hero.SparseSet.Unboxed qualified as SU
import Hero.System (System, withSetup)
import Hero.System.ComponentFunctions (addStore)
import Hero.World as World hiding (addStore)
import Optics.Core

newtype SparseSetSize = SparseSetSize {denseSize :: Word32}

-- | Component store backed by an unboxed sparse set. Can be used as a 'Store'.
newtype UnboxedSparseSet a = UnboxedSparseSet (SU.SparseSetUnboxed a)

-- | Creates an unboxed sparse set. The first parameter should be the maximum amount of live entities (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
unboxedSparseSet' ::
  forall component m i.
  (Unbox component, Component component, Store component ~ UnboxedSparseSet) =>
  SparseSetSize ->
  System i i
unboxedSparseSet' (SparseSetSize dense) =
  withSetup
    (\w -> let i = (coerce $ w ^. #maxEntities) in UnboxedSparseSet <$> SU.create @component i dense)
    addStore

-- | Creates an unboxed sparse set with the default size. If the component is not used by many entities, consider
-- decreasing the size.
unboxedSparseSet ::
  forall component m i.
  (Unbox component, Component component, Store component ~ UnboxedSparseSet) =>
  System i i
unboxedSparseSet =
  withSetup
    (\w -> let i = (coerce $ w ^. #maxEntities) in UnboxedSparseSet <$> SU.create @component i i)
    addStore

instance Unbox a => ComponentStore a UnboxedSparseSet where
  componentEntityDelete (UnboxedSparseSet set) entity = SU.remove set (coerce entity)

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

-- | Component store backed by a storable sparse set. Can be used as a 'Store'.
newtype StorableSparseSet a = StorableSparseSet (SV.SparseSetStorable a)

instance Storable a => ComponentStore a StorableSparseSet where
  componentEntityDelete (StorableSparseSet set) entity = SV.remove set (coerce entity)

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

-- | Creates a storable sparse set. The first parameter should be the maximum amount of live entities (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
storableSparseSet' ::
  forall component m i.
  (Storable component, Component component, Store component ~ StorableSparseSet) =>
  SparseSetSize ->
  System i i
storableSparseSet' (SparseSetSize dense) =
  withSetup
    (\w -> let i = (w ^. #maxEntities ^. coerced) in StorableSparseSet <$> SV.create @component i dense)
    addStore

-- | Creates a storable sparse set with the default size. If the component is not used by many entities, consider
-- decreasing the size.
storableSparseSet ::
  forall component m i.
  (Storable component, Component component, Store component ~ StorableSparseSet) =>
  System i i
storableSparseSet =
  withSetup
    (\w -> let i = (w ^. #maxEntities ^. coerced) in StorableSparseSet <$> SV.create @component i i)
    addStore

-- | Component store backed by a boxed sparse set. Can be used as a 'Store'. The storable version is faster and should be
-- used when possible.
newtype BoxedSparseSet a = BoxedSparseSet (SB.SparseSetBoxed a)

instance ComponentStore a BoxedSparseSet where
  componentEntityDelete (BoxedSparseSet set) entity = SB.remove set (coerce entity)

instance ComponentGet a BoxedSparseSet where
  componentContains (BoxedSparseSet set) entity = SB.contains set (coerce entity)
  componentGet (BoxedSparseSet set) entity = SB.unsafeLookup set (coerce entity)
  {-# INLINE componentContains #-}
  {-# INLINE componentGet #-}

instance ComponentPut a BoxedSparseSet where
  componentPut (BoxedSparseSet set) entity val = SB.insert set (coerce entity) val
  {-# INLINE componentPut #-}

instance ComponentDelete a BoxedSparseSet where
  componentDelete (BoxedSparseSet set) entity = SB.remove set (coerce entity)
  {-# INLINE componentDelete #-}

instance ComponentIterate a BoxedSparseSet where
  componentIterate (BoxedSparseSet set) f = SB.for set (coerce f)
  componentMembers (BoxedSparseSet set) = SB.size set
  {-# INLINE componentIterate #-}
  {-# INLINE componentMembers #-}

-- | Creates a boxed sparse set. The first parameter should be the maximum amount of live entities (size of the sparse
-- array) and the second should be the maximum amount of live entities for the component (size of the dense array).
boxedSparseSet' ::
  forall component m i.
  (Component component, Store component ~ BoxedSparseSet) =>
  SparseSetSize ->
  System i i
boxedSparseSet' (SparseSetSize dense) =
  withSetup
    (\w -> let i = (w ^. #maxEntities ^. coerced) in BoxedSparseSet <$> SB.create @component i dense)
    addStore

-- | Creates a boxed sparse set with the default size. If the component is not used by many entities, consider
-- decreasing the size.
boxedSparseSet ::
  forall component m i.
  (Component component, Store component ~ BoxedSparseSet) =>
  System i i
boxedSparseSet =
  withSetup
    (\w -> let i = (w ^. #maxEntities ^. coerced) in BoxedSparseSet <$> SB.create @component i i)
    addStore

instance (Store component ~ BoxedSparseSet) => AutomaticStoreCreation component BoxedSparseSet where
  createStoreAutomatically' = Just (\(MaxEntities i) -> BoxedSparseSet <$> SB.create @component i i)

instance (Storable component, Store component ~ StorableSparseSet) => AutomaticStoreCreation component StorableSparseSet where
  createStoreAutomatically' = Just (\(MaxEntities i) -> StorableSparseSet <$> SV.create @component i i)

instance (Unbox component, Store component ~ UnboxedSparseSet) => AutomaticStoreCreation component UnboxedSparseSet where
  createStoreAutomatically' = Just (\(MaxEntities i) -> UnboxedSparseSet <$> SU.create @component i i)