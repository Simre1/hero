module Hex
  ( -- * World
    World,
    newWorld,

    -- * Entity
    Entity,

    -- * Component
    Component(..),
    SparseSetStorableStore,
    SparseSetUnboxedStore,

    -- * System
    System,
    compileSystem,
    cmap,
    cmapM,
    cfold,
    cfoldM,
    cfoldl,
    cfoldr,
    newEntity,
    liftSystem,
    

    -- * Query
    Query,
    qput,
    qdelete,
    liftQuery,
    runQuery,
    runQuery_,
    singleQuery,
    singleQuery_,
    QCG,
    QCP,
    QCD,
    QCI,
    -- * Re-Exports
    Arrow(..)
  )
where

import Hex.Component
import Hex.Component.SparseSet
import Hex.Entity hiding (newEntity)
import Hex.System
import Hex.World
import Control.Arrow
