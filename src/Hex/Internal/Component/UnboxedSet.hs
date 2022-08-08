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
  !set <- SU.create entities (entities `quot` 3)
  pure $
    Store
      { storeContains = storeContains' set,
        storeGet = storeGet' set,
        storePut = storePut' set,
        storeDelete = storeDelete' set,
        storeFor = storeFor' set,
        storeMembers = storeMembers' set
      }

storeContains' :: Unboxable component => SU.SparseSetUnboxed component -> Entity -> IO Bool
storeContains' !set !entity = SU.contains set (coerce entity)
{-# INLINE storeContains' #-}

storeGet' :: Unboxable component => SU.SparseSetUnboxed component -> Entity -> IO component
storeGet' !set !entity = SU.unsafeLookup set (coerce entity)
{-# INLINE storeGet' #-}

storePut' :: Unboxable a => SU.SparseSetUnboxed a -> Entity -> a -> IO ()
storePut' !set !entity !val = SU.insert set (coerce entity) val
{-# INLINE storePut' #-}

storeDelete' :: Unboxable a => SU.SparseSetUnboxed a -> Entity -> IO ()
storeDelete' !set !entity = SU.remove set (coerce entity)
{-# INLINE storeDelete' #-}

storeFor' :: (Unboxable a) => SU.SparseSetUnboxed a -> (Entity -> IO ()) -> IO ()
storeFor' !set !f = SU.for set (coerce f)
{-# INLINE storeFor' #-}

storeMembers' :: SU.SparseSetUnboxed a -> IO Int
storeMembers' !set = SU.size set
{-# INLINE storeMembers' #-}

-- type UnboxComponent :: (Type -> Constraint,Type -> Constraint, Type -> Constraint)
-- type Unbox3 = Unbox
-- type Unbox1 = V.Vector VU.Vector
-- type Unbox2 = VM.MVector VMU.MVector