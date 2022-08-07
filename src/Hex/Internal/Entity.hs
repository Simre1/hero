module Hex.Internal.Entity where
import Data.Word

import qualified Data.Vector.Unboxed.Mutable as VU
import qualified Data.SparseSet.NoComponent as S
import Data.IORef
import Data.Coerce

newtype Entity = Entity Word32 deriving Show

newtype MaxEntities = MaxEntities Word32

data Entities = Entities !S.SparseSetNoComponent !(IORef Word32)

newEntities :: MaxEntities -> IO Entities
newEntities (MaxEntities max) = do
  set <- S.create max (max `quot` 3)
  lastId <- newIORef 0
  pure $ Entities set lastId


newEntity :: MaxEntities -> Entities -> IO Entity
newEntity (MaxEntities max) (Entities entities lastIdRef) = do
  lastId <- readIORef lastIdRef
  nextId <- findNext lastId 1
  writeIORef lastIdRef nextId
  pure $ Entity nextId
  where
    findNext last distance = do
      let next = last + distance `mod` max 
      contained <- S.contains entities next 
      if contained
        then findNext last ((distance + 1)^2)
        else pure next
{-# Inline newEntity #-}


removeEntity :: Entities -> Entity -> IO ()
removeEntity (Entities entities _) (Entity key) = S.remove entities key 
{-# Inline removeEntity #-}


forEntities :: Entities -> (Entity -> IO ()) -> IO ()
forEntities (Entities entities _) f = S.for entities (coerce f)
{-# Inline forEntities #-}
