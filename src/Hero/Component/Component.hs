{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hero.Component.Component where

import Data.Kind
import Hero.Entity (Entity, MaxEntities)
import Type.Reflection (Typeable)

-- | 'ComponentId' is unique to a component within the context of a World. Do not
-- use the same 'ComponentId' with different worlds.
newtype ComponentId = ComponentId {unwrapComponentId :: Int} deriving (Show, Eq, Ord)

-- | A component is a Haskell datatype usually containing raw data.
-- For example `data Position = Position Float Float`
class (ComponentStore component (Store component), Typeable component) => Component component where
  -- | The datastructure used to store the component (for example BoxedSparseSet)
  type Store component :: Type -> Type

  -- | Store which do not require systems to run can be automatically created (the sparse sets can be set up automatically!). Then, you do not have to
  -- set them up yourself and the defualt 'createStoreAutomatically' is enough.
  createStoreAutomatically :: Maybe (MaxEntities -> IO (Store' component))
  default createStoreAutomatically :: AutomaticStoreCreation component (Store component) => Maybe (MaxEntities -> IO (Store' component))
  createStoreAutomatically = createStoreAutomatically' @component @(Store component)

type Store' component = Store component component

-- | Contains methods that every store must have.
class ComponentStore component store where
  componentEntityDelete :: store component -> Entity -> IO ()

-- | Stores which do not any systems to run should implement AutomaticStoreCreation so
-- they can be set up automatically.
class AutomaticStoreCreation component store where
  createStoreAutomatically' :: Maybe (MaxEntities -> IO (Store' component))

instance {-# OVERLAPPABLE #-} AutomaticStoreCreation component store where
  createStoreAutomatically' = Nothing