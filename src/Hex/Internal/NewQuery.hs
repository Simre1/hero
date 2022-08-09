{-# LANGUAGE AllowAmbiguousTypes #-}
module Hex.Internal.NewQuery where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Hex.Internal.Component
import Hex.Internal.Entity
import Hex.Internal.World (World (..), worldComponentStorage)
import Control.Category
import Control.Arrow
import Prelude hiding ((.))

data Query m i o where
  QueryGet :: Component i => Query m () i
  QueryPut :: Query m a ()
  QueryDelete :: Query m a ()
  QueryConnect :: Query m a b -> Query m b c -> Query m a c
  QueryMap :: (a -> m b) -> Query m a b
  QueryCombine :: Query m i1 o1 -> Query m i2 o2 -> Query m (i1,i2) (o1,o2)

instance Applicative m => Category (Query m) where
  id = QueryMap pure
  (.) = flip QueryConnect

instance Applicative m => Arrow (Query m) where
  arr f = QueryMap (pure . f)
  (***) = QueryCombine

cmap :: (Component a, Applicative m) => (a -> b) -> Query m () ()
cmap f = QueryGet >>> arr f >>> QueryPut


newtype CompiledQuery m i a = CompiledQuery (i -> m a)



data QueryContext = QueryContext 


compileQuery :: World -> QueryContext -> Query m i a -> IO (CompiledQuery m i a)
compileQuery w q = case q of
  QueryGet -> undefined





type QC a = QueryComponent (S a) a

class QueryComponent (f :: Bool) component where
  queryContains :: World -> IO (Entity -> IO Bool)
  queryGet :: World -> IO (Entity -> IO component)
  -- queryPut :: World -> Entity -> component -> IO ()
  -- queryDelete :: World -> Entity -> IO ()
  -- queryFor :: World -> (Entity -> component -> IO ()) -> IO ()
  -- queryMembers :: World -> IO Int

-- type MP = MultiComponent

-- type MPS a = MP (S a) a

type family S a where
  S () = False
  S (a, b) = False
  S (a, b, c) = False
  S _ = True

-- instance QueryComponent False () where
--   multiContains w e = pure False
--   multiGet w e = pure ()
--   multiPut w e c = pure ()
--   multiDelete w e = pure ()
--   multiFor w f = pure ()
--   multiMembers w = pure 0
--   {-# INLINE multiContains #-}
--   {-# INLINE multiGet #-}
--   {-# INLINE multiPut #-}
--   {-# INLINE multiDelete #-}
--   {-# INLINE multiFor #-}
--   {-# INLINE multiMembers #-}
