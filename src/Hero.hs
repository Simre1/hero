-- |
-- Module      : Hero
-- Description : An ECS implementation in Haskell focused on performance
-- Copyright   : (c) Reitinger Simon, 2022
-- License     : MIT
module Hero
  ( -- * World
    World,
    createWorld,

    -- * Entity
    Entity,

    -- * Component
    Component (..),
    -- ** Premade components
    Position2D(..),
    -- ** Store
    MakeStore (..),
    StorableSparseSet,
    storableSparseSet,
    UnboxedSparseSet,
    unboxedSparseSet,
    BoxedSparseSet,
    boxedSparseSet,
    Global,
    makeGlobal,
    getGlobal,
    putGlobal,

    -- * System
    System,
    compileSystem,
    cmap,
    cmap',
    cmapM,
    cmapM',
    cfold,
    cfold',
    cfoldM,
    cfoldM',
    cfoldl,
    cfoldr,
    newEntity,
    deleteEntity,
    liftSystem,
    withSetup,

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
    Arrow (..),
    (>>>),
  )
where

import Control.Arrow
import Hero.Component
import Hero.Component.Basic
import Hero.Component.Store.Global
import Hero.Component.Store.SparseSet
import Hero.Entity hiding (newEntity)
import Hero.System
import Hero.World
